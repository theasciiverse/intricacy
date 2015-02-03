-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

{-# LANGUAGE FlexibleInstances #-}
module SDLUIMInstance () where

import Graphics.UI.SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Control.Concurrent.STM
import Control.Applicative hiding ((<*>))
import System.Timeout
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Word
import Data.Array
import Data.List
import Data.Function (on)
import Data.Foldable (for_)
--import Debug.Trace (traceShow)

import MainState
import Hex
import Command
import GameStateTypes
import Lock
import KeyBindings
import Mundanities
import Metagame
import Protocol
import Cache
import Database
import ServerAddr
import Util
import InputMode
import SDLRender
import SDLUI

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawMainState = do
	lift $ clearButtons >> clearSelectables
	s <- get
	let mode = ms2im s
	lift $ waitFrame
	drawMainState' s
	lift . drawTitle =<< getTitle
	lift $ do
	    drawButtons mode
	    drawUIOptionButtons mode
	    drawMsgLine
	    drawShortMouseHelp mode
	    refresh
	where
	drawMainState' (PlayState { psCurrentState=st, psLastAlerts=alerts, wrenchSelected=wsel }) = do
	    canUndo <- null <$> gets psGameStateMoveStack
	    canRedo <- null <$> gets psUndoneStack
	    lift $ do
		let selTools = [ idx |
			(idx, PlacedPiece pos p) <- enumVec $ placedPieces st
			, or [wsel && isWrench p, not wsel && isHook p] ]
		drawMainGameState selTools False alerts st
		registerUndoButtons canUndo canRedo
	drawMainState' (ReplayState { rsCurrentState=st } ) = do
	    canUndo <- null <$> gets rsGameStateMoveStack
	    canRedo <- null <$> gets rsMoveStack
	    lift $ do
		drawMainGameState [] False [] st
		registerUndoButtons canUndo canRedo
		renderToMain $ drawCursorAt Nothing
	drawMainState' (EditState { esGameStateStack=(st:sts), esUndoneStack=undostack,
		selectedPiece=selPiece, selectedPos=selPos }) = lift $ do
	    drawMainGameState (maybeToList selPiece) True [] st
	    renderToMain $ drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	    registerUndoButtons (null sts) (null undostack)
	    when (isJust selPiece) $ mapM_ registerButtonGroup
		[ singleButton (periphery 2 <+> 3<*>hw<+>hv) CmdDelete 0 [("delete",hu<+>neg hw)]
		, singleButton (periphery 2 <+> 3<*>hw) CmdMerge 4 [("merge",hu<+>neg hw)]
		]
	    sequence_
		[ when (null . filter (pred . placedPiece) . Vector.toList $ placedPieces st)
		    $ registerButton (periphery 0 <+> d) cmd 2 [("place",hu<+>neg hw),(tool,hu<+>neg hv)]
		| (pred,tool,cmd,d) <- [
		    (isWrench, "wrench", CmdTile $ WrenchTile zero, 3<*>hu <+> hv),
		    (isHook, "hook", CmdTile $ HookTile, 3<*>hu <+> 2<*>hv)] ]
	    drawPaintButtons
	drawMainState' (MetaState saddr undecls cOnly auth names _ _ rnamestvar _ _ mretired path mlock offset) = do
	    let ourName = authUser <$> auth
	    let selName = listToMaybe names
	    let home = isJust ourName && ourName == selName
	    lift $ renderToMain $ (erase >> drawCursorAt Nothing)
	    lift $ do
		maybe (drawEmptyMiniLock miniLockPos)
		    (\lock -> drawMiniLock lock miniLockPos)
		    (fst<$>mlock)
		registerSelectable miniLockPos 3 SelOurLock
	    lift $ when (not $ null path) $ do
		renderToMain $ renderStrColAtLeft messageCol path $ lockLinePos <+> hu
		registerSelectable (lockLinePos <+> 2<*>hu) 1 SelLockPath
		sequence_
		    [ registerButton (miniLockPos <+> 2<*>neg hv <+> hu <+> dv) cmd 4
			[(dirText,hu<+>neg hw),("lock",hu<+>neg hv)]
		    | (dv,cmd,dirText) <- [(zero,CmdPrevLock,"prev"),(neg hw,CmdNextLock,"next")] ]
	    lift $ do
		smallFont <- gets dispFontSmall
		renderToMain $ withFont smallFont $ renderStrColAtLeft messageCol
			(saddrStr saddr ++ if cOnly then " (cache only)" else "")
		    $ serverPos <+> hu

	    when (length names > 1) $ lift $ registerButton
		(codenamePos <+> 2<*>neg hu <+> hw) CmdBackCodename 0 [("back",2<*>hw<+>neg hv<+>neg hu)]

	    runMaybeT $ do
		name <- MaybeT (return selName)
		FetchedRecord fresh err muirc <- lift $ getUInfoFetched 300 name
		lift $ do
		    lift $ do
			unless fresh $ do
			    smallFont <- gets dispFontSmall
			    renderToMain $ withFont smallFont $ renderStrColAtLeft (opaquify $ dim errorCol) "(stale)" $ serverWaitPos
			maybe (return ()) (setMsgLineNoRefresh errorCol) err
			when (fresh && (isNothing ourName || home || isNothing muirc)) $
			    let reg = (isNothing muirc && isNothing ourName) || home
			    in registerButton (codenamePos <+> hw <+> 2<*>hu)
				(if reg then CmdRegister else CmdAuth) 2 [(if reg then "reg" else "auth", hu<+>neg hv)]
		    (if isJust muirc then drawName else drawNullName) name codenamePos
		    lift $ registerSelectable codenamePos 0 (SelSelectedCodeName name)
		    drawRelScore name (codenamePos<+>hu)
		    for_ muirc $ \(RCUserInfo (_,uinfo)) -> case mretired of
			    Just retired -> do
				fillArea locksPos
				    (map (locksPos<+>) $ zero:[rotate n $ 4<*>hu<->4<*>hw | n <- [0,2,3,5]])
				    [ \pos -> (lift $ registerSelectable pos 1 (SelOldLock ls)) >> drawOldLock ls pos
				    | ls <- retired ]
				lift $ registerButton retiredPos CmdShowRetired pubWheelAngle [("retired",hu<+>neg hw)]
				lift $ registerButton (retiredPos <+> hw) (CmdPlayLockSpec Nothing) 4 [("play",hu<+>neg hw),("no.",hu<+>neg hv)]
			    Nothing -> do
				sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) mlockinfo |
				    (i,mlockinfo) <- assocs $ userLocks uinfo ]
				let mmiscpos = (serverPos <+> 2<*>neg hv)
				when (isJust $ msum $ elems $ userLocks uinfo) $ lift $ do
				    registerButtonGroup
					([ Button mmiscpos (CmdSolve Nothing) [("solve",hu<+>neg hw),("lock",hu<+>neg hv)]
					, Button (mmiscpos<+>neg hv) (CmdViewSolution Nothing) [("view",hu<+>neg hw),("soln",hu<+>neg hv)]], (2,2))
				let tested = maybe False (isJust.snd) mlock
				when (isJust mlock && home) $ lift $ registerButton
				    (miniLockPos <+> 3<*>hv) (CmdPlaceLock Nothing) (if tested then 2 else 0) [("place",hv<+>5<*>neg hu),("lock",hw<+>4<*>neg hu)]
				lift $ registerButton retiredPos CmdShowRetired pubWheelAngle [("retired",hu<+>neg hw)]

	    when home $ do
		unless (null undecls) $ do
		    lift.renderToMain $ renderStrColAtLeft messageCol "Undeclared:" (undeclsPos<+>2<*>hv)
		    lift $ registerButton (undeclsPos<+>2<*>hv<+>neg hu) (CmdDeclare Nothing) 2 [("decl",hv<+>4<*>neg hu),("soln",hw<+>4<*>neg hu)]
		    fillArea (undeclsPos<+>hv)
			(map (undeclsPos<+>) $ hexDisc 1 ++ [hu<+>neg hw, neg hu<+>hv])
			[ \pos -> (lift $ registerSelectable pos 0 (SelUndeclared undecl)) >> drawActiveLock al pos
			| undecl@(Undeclared _ _ al) <- undecls ]
	    rnames <- liftIO $ atomically $ readTVar rnamestvar
	    unless (null rnames) $
		fillArea randomNamesPos
		    (map (randomNamesPos<+>) $ hexDisc 2)
		    [ \pos -> (lift $ registerSelectable pos 0 (SelRandom name)) >> drawName name pos
		    | name <- rnames ]

	    when (ourName /= selName) $ void $ runMaybeT $ do
		when (isJust ourName) $
		    lift.lift $ registerButton (codenamePos <+> neg hu <+> hw) CmdHome 4 [("home",hu<+>neg hv)]
		sel <- liftMaybe selName
		us <- liftMaybe ourName
		ourUInfo <- mgetUInfo us
		selUInfo <- mgetUInfo sel
		let accesses = map (uncurry getAccessInfo) [(ourUInfo,sel),(selUInfo,us)]
		lift $ do
		    fillArea (codenamePos<+>3<*>hv<+>hw) (map ((codenamePos<+>3<*>hv)<+>) [zero,hw,neg hv])
			[ \pos -> (lift $ registerSelectable pos 0 (SelScoreLock (Just sel) accessed $ ActiveLock us i)) >>
			    drawNameWithCharAndCol us white (lockIndexChar i) col pos
			| i <- [0..2]
			, let accessed = accesses !! 0 !! i
			, let col
				| accessed == Just AccessedPub = dim pubColour
				| isJust accessed = dim $ scoreColour $ -3
				| otherwise = obscure $ scoreColour 3 ]
		    fillArea (codenamePos<+>2<*>neg hw) (map ((codenamePos<+>3<*>neg hw)<+>) [zero,hw,neg hv])
			[ \pos -> (lift $ registerSelectable pos 0 (SelScoreLock Nothing accessed $ ActiveLock sel i)) >>
			    drawNameWithCharAndCol sel white (lockIndexChar i) col pos
			| i <- [0..2]
			, let accessed = accesses !! 1 !! i
			, let col
				| accessed == Just AccessedPub = obscure pubColour
				| isJust accessed = dim $ scoreColour $ 3
				| otherwise = obscure $ scoreColour $ -3 ]
		(usUnacc,selUnacc) <- MaybeT $ (snd<$>) <$> getRelScoreDetails sel
		lift.lift.renderToMain $ sequence_ [renderStrColAt (scoreColour score) (sign:show (abs score)) pos
			| (sign,score,pos) <-
			    [ ('-',usUnacc-3,codenamePos<+>3<*>hv<+>neg hw)
			    , ('+',3-selUnacc,codenamePos<+>3<*>neg hw<+>hv) ] ]
	drawMainState' _ = return ()

    drawMessage = say
    drawError = sayError

    reportAlerts = playAlertSounds

    getChRaw = do
	events <- liftIO getEvents
	case listToMaybe $ [ key | KeyDown key <- events ] of
	    Nothing -> getChRaw
	    Just (Keysym _ _ ch) -> return $ Just ch

    setUIBinding mode cmd ch =
	modify $ \s -> s { uiKeyBindings =
		Map.insertWith (\[bdg] -> \bdgs -> if bdg `elem` bdgs then delete bdg bdgs else bdg:bdgs)
		    mode [(ch,cmd)] $ uiKeyBindings s }

    getUIBinding mode cmd = ($cmd) <$> getBindingStr mode

    initUI = liftM isJust (runMaybeT $ do
	catchIOErrorMT $ SDL.init [InitVideo]
	catchIOErrorMT TTF.init
	lift $ do
	    readUIConfigFile
	    initVideo 0 0
	    liftIO $ setCaption "intricacy" "intricacy"
	    w <- gets scrWidth
	    h <- gets scrHeight
	    liftIO $ warpMouse (fromIntegral $ w`div`2) (fromIntegral $ h`div`2)
	    renderToMain $ erase
	    liftIO $ enableUnicode True
	    liftIO $ enableKeyRepeat 250 30
	    initAudio
	    readBindings
	)
	where
	    catchIOErrorMT m = MaybeT $ liftIO $ catchIO (m >> return (Just ())) (\_ -> return Nothing)

    endUI = do
	writeUIConfigFile
	writeBindings
	liftIO $ quit

    unblockInput = return $ pushEvent VideoExpose
    suspend = return ()
    redraw = return ()

    getDrawImpatience = do
	curState <- get
	let pos = serverWaitPos
	return $ \ticks -> void $ flip runStateT curState $ do
	    when (ticks>2) $ renderToMain $ do
		mapM (drawAtRel (filledHexGlyph $ bright black)) [ pos <+> i<*>hu | i <- [0..3] ]
		withFont (dispFontSmall curState) $
		    renderStrColAtLeft errorCol ("waiting..."++replicate (ticks`mod`3) '.') $ pos
	    refresh

    warpPointer pos = do
	(scrCentre, size) <- getGeom
	centre <- gets dispCentre
	let SVec x y = hexVec2SVec size (pos<->centre) <+> scrCentre
	liftIO $ warpMouse (fromIntegral x) (fromIntegral y)
	lbp <- gets leftButtonDown
	rbp <- gets rightButtonDown
	let [lbp',rbp'] = fmap (fmap (\_ -> (pos<->centre))) [lbp,rbp]
	modify $ \s -> s {leftButtonDown = lbp', rightButtonDown = rbp'}

    setYNButtons = do
	clearButtons
	registerButton (periphery 5 <+> hw) (CmdInputChar 'Y') 2 []
	registerButton (periphery 5 <+> neg hv) (CmdInputChar 'N') 0 []
	drawButtons IMTextInput
	refresh

    toggleColourMode = modify $ \s -> s {uiOptions = (uiOptions s){
        useFiveColouring = not $ useFiveColouring $ uiOptions s}}

    getInput mode = do
	fps <- gets fps
	events <- liftIO $ nubMouseMotions <$> getEventsTimeout (10^6`div`fps)
	oldUIState <- get
	cmds <- concat <$> mapM processEvent events
	setPaintFromCmds cmds
	addRefresh <- needRefresh oldUIState
	return $ cmds ++ if addRefresh then [CmdRefresh] else []
	where
	    nubMouseMotions evs =
		-- drop all but last mouse motion event
		let nubMouseMotions' False (mm@(MouseMotion {}):evs) = mm:(nubMouseMotions' True evs)
		    nubMouseMotions' True (mm@(MouseMotion {}):evs) = nubMouseMotions' True evs
		    nubMouseMotions' b (ev:evs) = ev:(nubMouseMotions' b evs)
		    nubMouseMotions' _ [] = []
		in reverse $ nubMouseMotions' False $ reverse evs
	    setPaintFromCmds cmds = sequence_
		[ modify $ \s -> s { paintTileIndex = pti }
		    | (pti,pt) <- zip [0..] paintTiles
		    , cmd <- cmds
		    , (isNothing pt && cmd == CmdDelete) ||
			(isJust $ do
			    pt' <- pt
			    CmdTile t <- Just cmd
			    guard $ ((==)`on`tileType) t pt') ]
	    needRefresh old = do
		new <- get
		now <- liftIO getTicks
		anim <- maybe False (<now) <$> gets nextAnimFrameAt
		return $ anim || uistatesMayVisiblyDiffer old new

	    uistatesMayVisiblyDiffer uis1 uis2 =
		uis1 { mousePos = (zero,False), lastFrameTicks=0 }
		/= uis2 {mousePos = (zero,False), lastFrameTicks=0 }
	    processEvent (KeyDown (Keysym _ _ ch)) = case mode of
		IMTextInput -> return [CmdInputChar ch]
		_ -> do
		    setting <- gets settingBinding
		    if isJust setting && ch /= '\0'
		    then do
			modify $ \s -> s {settingBinding = Nothing}
			when (ch /= '\ESC') $ setUIBinding mode (fromJust setting) ch
			return []
		    else do
			uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
			let mCmd = lookup ch $ uibdgs ++ bindings mode
			return $ maybeToList mCmd
	    processEvent (MouseMotion {}) = do
		(oldMPos,_) <- gets mousePos
		(pos@(mPos,_),(sx,sy,sz)) <- getMousePos
		updateMousePos mode pos
		lbp <- gets leftButtonDown
		rbp <- gets rightButtonDown
		centre <- gets dispCentre
		let drag :: Maybe HexVec -> Maybe Command
		    drag bp = do
			fromPos@(HexVec x y z) <- bp
			-- check we've dragged at least a full hex's distance:
			guard $ not.all (\(a,b) -> abs ((fromIntegral a) - b) < 1.0) $ [(x,sx),(y,sy),(z,sz)]
			let dir = hexVec2HexDirOrZero $ mPos <-> fromPos
			guard $ dir /= zero
			return $ CmdDrag (fromPos<+>centre) dir
		case mode of
		    IMEdit -> case drag rbp of
			Just cmd -> return [cmd]
			Nothing -> if mPos /= oldMPos
			    then do
				pti <- getEffPaintTileIndex
				return $ [ CmdMoveTo $ mPos <+> centre ] ++
				    (if isJust lbp then [ CmdPaintFromTo (paintTiles!!pti) (oldMPos<+>centre) (mPos<+>centre) ] else [])
			    else return []
		    IMPlay -> return $ maybeToList $ msum $ map drag [lbp, rbp]
		    _ -> return []
		where
		    mouseFromTo from to = do
			let dir = hexVec2HexDirOrZero $ to <-> from
			if dir /= zero
			    then (CmdDir WHSSelected dir:) <$> mouseFromTo (from <+> dir) to
			    else return []
	    processEvent (MouseButtonDown _ _ ButtonLeft) = do
		pos@(mPos,central) <- gets mousePos
		modify $ \s -> s { leftButtonDown = Just mPos }
		rb <- isJust <$> gets rightButtonDown
		mcmd <- cmdAtMousePos pos mode (Just False)
		let hotspotAction = listToMaybe
			$ map (\cmd -> return [cmd]) (maybeToList mcmd)
			++ [ (modify $ \s -> s {paintTileIndex = i}) >> return []
			    | i <- take (length paintTiles) [0..]
			    , mPos == paintButtonStart <+> i<*>hv ]
			++ [ toggleUIOption uiOB1 >> updateHoverStr mode >> return []
			    | mPos == uiOptPos uiOB1 ]
			++ [ toggleUIOption uiOB2 >> updateHoverStr mode >> return []
			    | mPos == uiOptPos uiOB2 ]
			++ [ toggleUIOption uiOB3 >> updateHoverStr mode >> return []
			    | mPos == uiOptPos uiOB3 ]
			++ [ toggleUIOption uiOB4 >> updateHoverStr mode >> return []
			    | mPos == uiOptPos uiOB4 ]
			++ [ toggleUIOption uiOB5 >> updateHoverStr mode >> return []
			    | mPos == uiOptPos uiOB5 ]

		if rb
		then return [ CmdWait ]
		else flip fromMaybe hotspotAction $ case mode of
		    IMEdit -> do
			pti <- getEffPaintTileIndex
			return $ [ drawCmd (paintTiles!!pti) False ]
		    IMPlay -> do
			centre <- gets dispCentre
			return $ [ CmdManipulateToolAt $ mPos <+> centre ]
		    _ -> return []
	    processEvent (MouseButtonUp _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = Nothing }
		return []
	    processEvent (MouseButtonDown _ _ ButtonRight) = do
		pos@(mPos,_) <- gets mousePos
		modify $ \s -> s { rightButtonDown = Just mPos }
		lb <- isJust <$> gets leftButtonDown
		if lb
		then return [ CmdWait ]
		else (fromMaybe [] <$>) $ runMaybeT $ msum
		    [ do
			cmd <- MaybeT $ cmdAtMousePos pos mode Nothing
			guard $ mode /= IMTextInput
			modify $ \s -> s { settingBinding = Just cmd }
			return []
		    , do
			cmd <- MaybeT $ cmdAtMousePos pos mode (Just True)
			return [cmd]
		    , case mode of
			IMPlay -> return [ CmdWait ]
			_ -> return [ CmdSelect ] ]
	    processEvent (MouseButtonUp _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = Nothing }
		return [ CmdUnselect ]
	    processEvent (MouseButtonDown _ _ ButtonWheelUp) = doWheel 1
	    processEvent (MouseButtonDown _ _ ButtonWheelDown) = doWheel $ -1
	    processEvent (MouseButtonDown _ _ ButtonMiddle) = do
		(mPos,_) <- gets mousePos
		modify $ \s -> s { middleButtonDown = Just mPos }
		rb <- isJust <$> gets rightButtonDown
		return $ if rb then [ CmdDelete ] else []
	    processEvent (MouseButtonUp _ _ ButtonMiddle) = do
		modify $ \s -> s { middleButtonDown = Nothing }
		return []
	    processEvent (VideoResize w h) = do
		initVideo w h
		return [ CmdRedraw ]
	    processEvent VideoExpose = return [ CmdRedraw ]
	    processEvent Quit = return [ CmdForceQuit ]

	    processEvent _ = return []

	    doWheel dw = do
		rb <- isJust <$> gets rightButtonDown
		mb <- isJust <$> gets middleButtonDown
		if ((rb || mb || mode == IMReplay) && mode /= IMEdit)
		    || (mb && mode == IMEdit)
		then return [ if dw == 1 then CmdUndo else CmdRedo ]
		else if mode /= IMEdit || rb
		then return [ CmdRotate WHSSelected dw ]
		else do
		    modify $ \s -> s { paintTileIndex = (paintTileIndex s + dw) `mod` (length paintTiles) }
		    return []


	    drawCmd mt True = CmdPaint mt
	    drawCmd (Just t) False = CmdTile t
	    drawCmd Nothing _ = CmdDelete

	    getMousePos :: UIM ((HexVec,Bool),(Double,Double,Double))
	    getMousePos = do
		(scrCentre, size) <- getGeom
		(x,y,_) <- lift getMouseState
		let sv = (SVec (fromIntegral x) (fromIntegral y)) <+> neg scrCentre
		let mPos@(HexVec x y z) = sVec2HexVec size sv
		let (sx,sy,sz) = sVec2dHV size sv
		let isCentral = all (\(a,b) -> abs ((fromIntegral a) - b) < 0.5) $
			[(x,sx),(y,sy),(z,sz)]
		return ((mPos,isCentral),(sx,sy,sz))
	    updateMousePos mode newPos = do
		oldPos <- gets mousePos
		when (newPos /= oldPos) $ do
		    modify $ \ds -> ds { mousePos = newPos }
		    updateHoverStr mode

    showHelp mode = do
	bdgs <- nub <$> getBindings mode
	smallFont <- gets dispFontSmall
	renderToMain $ do
	    erase
	    let bdgWidth = (screenWidthHexes-6) `div` 3
		showKeys chs = intercalate "/" (map showKeyFriendly chs)
		maxkeyslen = maximum . (0:) $ map (length.showKeys.map fst) $ groupBy ((==) `on` snd) bdgs
		mouseHelpStrs = ["Mouse commands:", "Right-click on a button to set a keybinding;"]
		    ++ case mode of
			IMPlay -> ["Click on tool to select, drag to move;",
			    "Click by tool to move; right-click to wait;", "Scroll wheel to rotate hook;",
			    "Scroll wheel with right button held down to undo/redo."]
			IMEdit -> ["Left-click to draw selected; scroll to change selection;",
			    "Right-click on piece to select, drag to move;",
			    "While holding right-click: left-click to advance time, middle-click to delete;",
			    "Scroll wheel to rotate selected piece; scroll wheel while held down to undo/redo."]
			IMReplay -> ["Scroll wheel with right button held down to undo/redo."]
			IMMeta -> ["Left-clicking on something does most obvious thing;",
			    "Right-clicking does second-most obvious thing."]
	    renderStrColAt messageCol "Keybindings:" $ (screenHeightHexes`div`4)<*>(hv<+>neg hw)
	    sequence_ [ with $ renderStrColAtLeft messageCol
			( keysStr ++ ": " ++ desc )
			$ (x*bdgWidth-(screenWidthHexes-6)`div`2)<*>hu <+> neg hv <+>
			  (screenHeightHexes`div`4 - y`div`2)<*>(hv<+>neg hw) <+>
			  (y`mod`2)<*>hw
		| ((keysStr,with,desc),(x,y)) <- zip [(keysStr,with,desc)
			| group <- groupBy ((==) `on` snd) $ sortBy (compare `on` snd) bdgs
			, let cmd = snd $ head group
			, let desc = describeCommand cmd
			, not $ null desc
			, let chs = map fst group
			, let keysStr = showKeys chs
			, let with = if True -- 3*(bdgWidth-1) < length desc + length keysStr + 1
				then withFont smallFont
				else id
			]
		    (map (`divMod` (screenHeightHexes-9)) [0..])
		, (x+1)*bdgWidth < screenWidthHexes]
	    sequence_ [ renderStrColAt messageCol str
			$ (3-screenHeightHexes`div`4-y`div`2)<*>(hv<+>neg hw)
			  <+> hv
			  <+> (y`mod`2)<*>hw
		| (str,y) <- zip mouseHelpStrs [0..] ]
	refresh

drawShortMouseHelp mode = do
    mwhs <- gets $ whsButtons.uiOptions
    showBT <- showButtonText <$> gets uiOptions
    when (showBT && isNothing mwhs) $ do
	let helps = shortMouseHelp mode
	smallFont <- gets dispFontSmall
	renderToMain $ withFont smallFont $ sequence_
	    [ renderStrColAtLeft (dim cyan) help
		(periphery 3 <+> neg hu <+> (2-n)<*>hv )
	    | (n,help) <- zip [0..] helps ]
    where
	shortMouseHelp IMPlay =
	    [ "LMB: select/move tool"
	    , "LMB+drag: move tool"
	    , "Wheel: turn hook"
	    , "RMB: wait a turn"
	    , "RMB+Wheel: undo/redo"
	    ]
	shortMouseHelp IMEdit =
	    [ "LMB: paint; Ctrl+LMB: delete"
	    , "Wheel: set paint type"
	    , "RMB: select piece; drag to move"
	    , "RMB+LMB: wait; RMB+MMB: delete piece"
	    , "MMB+Wheel: undo/redo"
	    ]
	shortMouseHelp IMReplay =
	    [ "Wheel: advance/regress time" ]
	shortMouseHelp _ = []

getEvents = do
    e <- waitEvent
    es <- pollEvents
    return $ e:es

getEventsTimeout us = do
    es <- maybeToList <$> timeout us waitEvent
    es' <- pollEvents
    return $ es++es'

pollEvents = do
    e <- pollEvent
    case e of
	NoEvent -> return []
	_ -> do
	    es <- pollEvents
	    return $ e:es

updateHoverStr :: InputMode -> UIM ()
updateHoverStr mode = do
    p@(mPos,isCentral) <- gets mousePos
    showBT <- showButtonText <$> gets uiOptions
    hstr <- runMaybeT $ msum
	[ MaybeT ( cmdAtMousePos p mode Nothing ) >>= lift . describeCommandAndKeys
	, guard showBT >> MaybeT (helpAtMousePos p mode)
	, guard (showBT && mode == IMEdit) >> msum
	    [ return $ "set paint mode: " ++ describeCommand (paintTileCmds!!i)
	    | i <- take (length paintTiles) [0..]
	    , mPos == paintButtonStart <+> i<*>hv ]
	, guard (mPos == uiOptPos uiOB1) >> describeUIOptionButton uiOB1
	, guard (mPos == uiOptPos uiOB2) >> describeUIOptionButton uiOB2
	, guard (mPos == uiOptPos uiOB3) >> describeUIOptionButton uiOB3
	, guard (mPos == uiOptPos uiOB4) >> describeUIOptionButton uiOB4
	, guard (mPos == uiOptPos uiOB5) >> describeUIOptionButton uiOB5 ]
    modify $ \ds -> ds { hoverStr = hstr }
    where
	describeCommandAndKeys :: Command -> UIM String
	describeCommandAndKeys cmd = do
	    uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
	    return $ describeCommand cmd ++ " ["
		++ concat (intersperse ","
		    (map showKeyFriendly $ findBindings (uibdgs ++ bindings mode) cmd))
		++ "]"


fillArea :: HexVec -> [HexVec] -> [HexVec -> MainStateT UIM ()] -> MainStateT UIM ()
fillArea centre area draws = do
    offset <- gets listOffset
    let na = length area
	listButton cmd = \pos -> lift $ registerButton pos cmd 4 []
	draws' = if offset > 0 && length draws > na
	    then listButton CmdPrevPage :
		drop (max 0 $ min (length draws - (na-1)) (na-1 + (na-2)*(offset-1))) draws
	    else draws
	selDraws = if length draws' > na
	    then take (na-1) draws' ++ [listButton CmdNextPage]
	    else take na draws'
    sequence_ $ map (uncurry ($)) $
	zip selDraws $ sortBy (compare `on` hexVec2SVec 37) $
	    take (length selDraws) $ sortBy
		(compare `on` (hexLen . (<->centre)))
		area

drawOldLock ls pos = void.runMaybeT $ msum [ do
	lock <- mgetLock ls
	lift.lift $ drawMiniLock lock pos
    , lift.lift.renderToMain $
	    renderStrColAt messageCol (show ls) pos
    ]


drawName name pos = nameCol name >>= drawNameCol name pos
drawNullName name pos = drawNameCol name pos $ invisible white
drawNameCol name pos col = do
    lift.renderToMain $ do
	drawAtRel (playerGlyph col) pos
	renderStrColAt messageCol name pos
drawRelScore name pos = do
    col <- nameCol name
    relScore <- getRelScore name
    flip (maybe (return ())) relScore $ \score ->
	lift $ do
	    renderToMain $ renderStrColAt col
		((if score > 0 then "+" else "") ++ show score) pos
	    registerSelectable pos 0 (SelRelScore score)

drawNote note pos = case noteBehind note of
    Just al -> drawActiveLock al pos
    Nothing -> drawPublicNote (noteAuthor note) pos
drawActiveLock al@(ActiveLock name i) pos = do
    accessed <- accessedAL al
    drawNameWithChar name
	(if accessed then accColour else white)
	(lockIndexChar i) pos
drawPublicNote name =
    drawNameWithChar name pubColour 'P'
drawNameWithChar name charcol char pos = do
    col <- nameCol name
    drawNameWithCharAndCol name charcol char col pos
drawNameWithCharAndCol name charcol char col pos = do
    size <- fromIntegral.snd <$> lift getGeom
    let up = SVec 0 $ - (ysize size - size`div`2)
    let down = SVec 0 $ ysize size
    smallFont <- lift $ gets dispFontSmall
    lift.renderToMain $ do
	drawAtRel (playerGlyph col) pos
	displaceRender up $
	    renderStrColAt messageCol name pos
	displaceRender down $ withFont smallFont $
	    renderStrColAt charcol [char] pos
pubWheelAngle = 5
pubColour = colourWheel pubWheelAngle -- ==purple
accColour = cyan
nameCol name = do
    ourName <- (authUser <$>) <$> gets curAuth
    relScore <- getRelScore name
    return $ dim $ case relScore of
	    Nothing -> Pixel $ if ourName == Just name then 0xc0c0c000 else 0x80808000
	    Just score -> scoreColour score
scoreColour :: Int -> Pixel
scoreColour score = Pixel $ case score of
	0 -> 0x80800000
	1 -> 0x70a00000
	2 -> 0x40c00000
	3 -> 0x00ff0000
	(-1) -> 0xa0700000
	(-2) -> 0xc0400000
	(-3) -> 0xff000000

drawLockInfo :: ActiveLock -> Maybe LockInfo -> MainStateT UIM ()
drawLockInfo al@(ActiveLock name i) Nothing = do
    let centre = hw<+>neg hv <+> 7*(i-1)<*>hu
    lift $ drawEmptyMiniLock centre
    drawNameWithCharAndCol name white (lockIndexChar i) (invisible white) centre
    ourName <- (authUser <$>) <$> gets curAuth
    lift $ registerSelectable centre 3 $ SelLockUnset (ourName == Just name) al
drawLockInfo al@(ActiveLock name i) (Just lockinfo) = do
    let centre = locksPos <+> 7*(i-1)<*>hu
    let accessedByPos = centre <+> 3<*>(hv <+> neg hw)
    let accessedPos = centre <+> 2<*>(hw <+> neg hv)
    let notesPos = centre <+> 3<*>(hw <+> neg hv)
    ourName <- (authUser <$>) <$> gets curAuth
    runMaybeT $ msum [
	do
	    lock <- mgetLock $ lockSpec lockinfo
	    lift.lift $ do
		drawMiniLock lock centre
		registerSelectable centre 3 $ SelLock al
	, lift $ do
		drawActiveLock al centre
		lift $ registerSelectable centre 3 $ SelLock al
	]

    size <- snd <$> lift getGeom
    lift $ do
	renderToMain $ displaceRender (SVec size 0) $ renderStrColAt (brightish white) "PRIVY" $ accessedByPos <+> hv
	registerSelectable (accessedByPos <+> hv) 0 SelPrivyHeader
	registerSelectable (accessedByPos <+> hv <+> hu) 0 SelPrivyHeader
    if public lockinfo
    then lift $ do
	renderToMain $ renderStrColAt pubColour "All" accessedByPos
	registerSelectable accessedByPos 1 SelPublicLock
    else if null $ accessedBy lockinfo
	then lift.renderToMain $ renderStrColAt dimWhiteCol "No-one" accessedByPos
	else fillArea accessedByPos
		[ accessedByPos <+> d | j <- [0..2], i <- [-2..3]
		    , i-j > -4, i-j < 3
		    , let d = j<*>hw <+> i<*>hu ]
		$ [ \pos -> (lift $ registerSelectable pos 0 (SelSolution note)) >> drawNote note pos
		    | note <- lockSolutions lockinfo ] ++
		[ \pos -> (lift $ registerSelectable pos 0 (SelAccessed name)) >> drawName name pos
		    | name <- accessedBy lockinfo \\ map noteAuthor (lockSolutions lockinfo) ]

    undecls <- gets undeclareds
    case if isJust $ guard . (|| public lockinfo) . (`elem` map noteAuthor (lockSolutions lockinfo)) =<< ourName
	    then if public lockinfo
		then Just (pubColour,"Accessed!",AccessedPublic)
		else Just (accColour, "Solved!",AccessedSolved)
	    else if any (\(Undeclared _ ls _) -> ls == lockSpec lockinfo) undecls
		then Just (yellow, "Undeclared",AccessedUndeclared)
		else Nothing
	of
	Just (col,str,selstr) -> lift $ do
	    renderToMain $ renderStrColAt col str accessedPos
	    registerSelectable accessedPos 1 (SelAccessedInfo selstr)
	Nothing -> do
	    read <- take 3 <$> getNotesReadOn lockinfo
	    unless (ourName == Just name) $ do
		let readPos = accessedPos <+> (-3)<*>hu
		lift.renderToMain $ renderStrColAt (if length read == 3 then accColour else dimWhiteCol)
		    "Read:" $ readPos
		when (length read == 3) $ lift $ registerSelectable readPos 0 (SelAccessedInfo AccessedReadNotes)
		fillArea (accessedPos<+>neg hu) [ accessedPos <+> i<*>hu | i <- [-1..1] ]
		    $ take 3 $ [ \pos -> (lift $ registerSelectable pos 0 (SelReadNote note)) >> drawName (noteAuthor note) pos
			| note <- read ] ++ (repeat $ \pos -> (lift $ registerSelectable pos 0 SelReadNoteSlot >>
				renderToMain (drawAtRel (hollowGlyph $ dim green) pos)))

    lift $ do
	renderToMain $ displaceRender (SVec size 0) $ renderStrColAt (brightish white) "NOTES" $ notesPos <+> hv
	registerSelectable (notesPos <+> hv) 0 SelNotesHeader
	registerSelectable (notesPos <+> hv <+> hu) 0 SelNotesHeader
    if null $ notesSecured lockinfo
	then lift.renderToMain $ renderStrColAt dimWhiteCol "None" notesPos
	else fillArea notesPos
		[ notesPos <+> d | j <- [0..2], i <- [-2..3]
		    , i-j > -4, i-j < 3
		    , let d = j<*>hw <+> i<*>hu ]
		[ \pos -> (lift $ registerSelectable pos 0 (SelSecured note)) >> drawActiveLock (noteOn note) pos
		    | note <- notesSecured lockinfo ]

