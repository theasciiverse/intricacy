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
import qualified Data.Map as Map
import Data.Map (Map)
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
		[ singleButton (periphery 2 <+> 3<*>hw<+>hu) CmdDelete 0
		-- , singleButton (periphery 2 <+> 3<*>hw) CmdMerge 4
		]
	    drawPaintButtons
	drawMainState' (MetaState saddr undecls cOnly auth names _ _ rnamestvar _ _ mretired path mlock offset) = do
	    let ourName = authUser <$> auth
	    let selName = listToMaybe names
	    let home = isJust ourName && ourName == selName
	    lift $ renderToMain $ (erase >> drawCursorAt Nothing)
	    lift $ maybe (drawEmptyMiniLock miniLockPos)
		((registerSelectable miniLockPos 3 SelOurLock >>) . flip drawMiniLock miniLockPos)
		(fst<$>mlock)
	    lift $ do
		smallFont <- gets dispFontSmall
		renderToMain $ withFont smallFont $ renderStrColAtLeft messageCol
			(saddrStr saddr ++ if cOnly then " (cache only)" else "")
		    $ serverPos <+> hu
		renderToMain $ renderStrColAtLeft messageCol path $ lockLinePos <+> hu
		when (offset>0) $
		    registerButtonGroup $ singleButton (nextPagePos<+>neg hu) CmdPrevPage 4

	    when (length names > 1) $ lift $ registerButtonGroup $ singleButton
		(codenamePos <+> 2<*>neg hu <+> hw) CmdBackCodename 0

	    runMaybeT $ do
		name <- MaybeT (return selName)
		FetchedRecord fresh err muirc <- lift $ getUInfoFetched 300 name
		lift $ do
		    lift $ do
			unless fresh $ do
			    smallFont <- gets dispFontSmall
			    renderToMain $ withFont smallFont $ renderStrColAtLeft (opaquify $ dim errorCol) "(stale)" $ serverWaitPos
			maybe (return ()) (setMsgLineNoRefresh errorCol) err
			when (fresh && (isNothing ourName || home)) $
			    registerButtonGroup $ singleButton (codenamePos <+> 2<*>hu)
				(if isNothing muirc || home then CmdRegister else CmdAuth) 2
		    (if isJust muirc then drawName else drawNullName) name codenamePos
		    lift $ registerSelectable codenamePos 0 SelSelectedCodeName
		    drawRelScore name (codenamePos<+>hu)
		    for_ muirc $ \(RCUserInfo (_,uinfo)) -> case mretired of 
			    Just retired -> do
				fillArea locksPos
				    (map (locksPos<+>) $ zero:[rotate n $ 4<*>hu<->4<*>hw | n <- [0,2,3,5]])
				    [ \pos -> (lift $ registerSelectable pos 1 (SelOldLock ls)) >> drawOldLock ls pos
				    | ls <- retired ]
				lift $ registerButtonGroup $ singleButton retiredPos CmdShowRetired pubWheelAngle
				lift $ registerButtonGroup $ singleButton (retiredPos <+> hw) (CmdPlayLockSpec Nothing) 4
			    Nothing -> do
				sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) mlockinfo |
				    (i,mlockinfo) <- assocs $ userLocks uinfo ]
				let mmiscpos = (serverPos <+> 2<*>neg hv)
				when (isJust $ msum $ elems $ userLocks uinfo) $ lift $ do
				    registerButtonGroup
					([ Button mmiscpos (CmdSolve Nothing), Button (mmiscpos<+>hu) (CmdViewSolution Nothing)], (2,2))
				let tested = maybe False (isJust.snd) mlock
				when (isJust mlock && home) $ lift $ registerButtonGroup $
					singleButton (miniLockPos <+> 2<*>hv <+> neg hu) (CmdPlaceLock Nothing) $ if tested then 2 else 0
				lift $ registerButtonGroup $ singleButton retiredPos CmdShowRetired pubWheelAngle

	    when home $ do
		unless (null undecls) $ do
		    lift.renderToMain $ renderStrColAtLeft messageCol "Undeclared:" (undeclsPos<+>2<*>hv)
		    lift $ registerButtonGroup $ singleButton (undeclsPos<+>2<*>hv<+>neg hu) (CmdDeclare Nothing) 2
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
		    lift.lift $ registerButtonGroup $ singleButton (codenamePos <+> neg hu <+> hw) CmdHome 4
		sel <- MaybeT $ return selName
		us <- MaybeT $ return ourName
		ourUInfo <- mgetUInfo us
		selUInfo <- mgetUInfo sel
		let accesses = map (uncurry getAccessInfo) [(ourUInfo,sel),(selUInfo,us)]
		lift $ do 
		    fillArea (codenamePos<+>3<*>hv<+>hw) (map ((codenamePos<+>3<*>hv)<+>) [zero,hw,neg hv])
			[ \pos -> (lift $ registerSelectable pos 0 SelOurAL) >>
			    drawNameWithCharAndCol us white (lockIndexChar i) col pos
			| i <- [0..2]
			, let (pub,access) = accesses !! 0 !! i
			, let col
				| pub = obscure pubColour 
				| access = obscure $ scoreColour $ -3 
				| otherwise = dim $ scoreColour 3 ]
		    fillArea (codenamePos<+>2<*>neg hw) (map ((codenamePos<+>3<*>neg hw)<+>) [zero,hw,neg hv])
			[ \pos -> (lift $ registerSelectable pos 0 (SelLock $ ActiveLock sel i)) >>
			    drawNameWithCharAndCol sel white (lockIndexChar i) col pos
			| i <- [0..2]
			, let (pub,access) = accesses !! 1 !! i
			, let col
				| pub = obscure pubColour 
				| access = obscure $ scoreColour $ 3 
				| otherwise = dim $ scoreColour $ -3 ]
	drawMainState' _ = return ()

    drawMessage = say
    drawError = sayError

    drawAlerts alerts = return ()

    showHelp _ = return ()

    initUI = liftM isJust (runMaybeT $ do
	catchIOErrorMT $ SDL.init [InitVideo]
	catchIOErrorMT TTF.init
	lift $ do
	    modes <- liftIO $ listModes Nothing [Resizable]
	    let Rect _ _ w h = case modes of
		    Modes (mode:_) -> mode
		    AnyOK -> Rect 0 0 1024 768
		    _ -> error "SDL says no resolution available"
	    initVideo w h
	    liftIO $ warpMouse (fromIntegral $ w`div`2) (fromIntegral $ h`div`2)
	    renderToMain $ erase
	    liftIO $ enableUnicode True
	    liftIO $ enableKeyRepeat 250 30
	    readUIConfigFile
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

    getInput mode = do
	events <- liftIO $ getEvents
	oldUIState <- get
	cmds <- concat <$> mapM processEvent events
	newUIState <- get
	return $ cmds ++ if uistatesMayVisiblyDiffer oldUIState newUIState then [CmdRefresh] else []
	where
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
			when (ch /= '\ESC') $
			    modify $ \s -> s { uiKeyBindings =
				Map.insertWith (++) mode [(ch,fromJust setting)] $ uiKeyBindings s }
			return []
		    else do
			uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
			let mCmd = lookup ch $ uibdgs ++ bindings mode
			sequence_
			    [ modify $ \s -> s { paintTileIndex = pti }
				| (pti,pt) <- zip [0..] paintTiles
				, (isNothing pt && mCmd == Just CmdWait) ||
				    (isJust $ do
					pt' <- pt
					CmdTile t <- mCmd
					guard $ ((==)`on`tileType) t pt') ]
			return $ maybeToList mCmd
	    processEvent (MouseMotion {}) = do
		(oldMPos,_) <- gets mousePos
		updateMousePos mode
		(mPos,_) <- gets mousePos
		if mode == IMEdit && mPos /= oldMPos then do
			relMoves <- mouseFromTo oldMPos mPos
			lb <- gets leftButtonDown
			pti <- getEffPaintTileIndex
			centre <- gets dispCentre
			return $ relMoves ++ [ CmdMoveTo $ mPos <+> centre ] ++
			    (if lb then [ CmdPaintFromTo (paintTiles!!pti) (oldMPos<+>centre) (mPos<+>centre) ] else [])
		    else return []
		where
		    mouseFromTo from to = do
			let dir = hexVec2HexDirOrZero $ to <-> from
			if dir /= zero
			    then (CmdDir WHSSelected dir:) <$> mouseFromTo (from <+> dir) to
			    else return []
	    processEvent (MouseButtonDown _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = True }
		pos@(mPos,central) <- gets mousePos
		rb <- gets rightButtonDown
		mcmd <- cmdAtMousePos pos mode (Just False)
		let hotspotAction = listToMaybe
			$ map (\cmd -> return [cmd]) (maybeToList mcmd)
			++ [ (modify $ \s -> s {paintTileIndex = i}) >> return []
			    | i <- take (length paintTiles) [0..]
			    , mPos == paintButtonStart <+> i<*>hv ]
			++ [ toggleUIOption uiOB1 >> updateHoverStr >> return []
			    | mPos == uiOptPos uiOB1 ]
			++ [ toggleUIOption uiOB2 >> updateHoverStr >> return []
			    | mPos == uiOptPos uiOB2 ]
			++ [ toggleUIOption uiOB3 >> updateHoverStr >> return []
			    | mPos == uiOptPos uiOB3 ]

		if rb
		then return [ CmdWait ]
		else flip fromMaybe hotspotAction $ do
		    pti <- getEffPaintTileIndex
		    return $ [ drawCmd (paintTiles!!pti) False ]
	    processEvent (MouseButtonUp _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = False }
		return []
	    processEvent (MouseButtonDown _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = True }
		pos <- gets mousePos
		(fromMaybe [] <$>) $ runMaybeT $ msum
		    [ do
			cmd <- MaybeT $ cmdAtMousePos pos mode Nothing
			modify $ \s -> s { settingBinding = Just cmd }
			return []
		    , do
			cmd <- MaybeT $ cmdAtMousePos pos mode (Just True)
			return [cmd]
		    , return [ CmdSelect ] ]
	    processEvent (MouseButtonUp _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = False }
		return [ CmdUnselect ]
	    processEvent (MouseButtonDown _ _ ButtonWheelUp) = doWheel 1
	    processEvent (MouseButtonDown _ _ ButtonWheelDown) = doWheel $ -1
	    processEvent (MouseButtonDown _ _ ButtonMiddle) = do
		modify $ \s -> s { middleButtonDown = True }
		rb <- gets rightButtonDown
		return $ if rb then [ CmdDelete ] else []
	    processEvent (MouseButtonUp _ _ ButtonMiddle) = do
		modify $ \s -> s { middleButtonDown = False }
		return []
	    processEvent (VideoResize w h) = do
		initVideo w h
		return [ CmdRedraw ]
	    processEvent VideoExpose = return [ CmdRedraw ]
		
	    processEvent _ = return []

	    doWheel dw = do
		rb <- gets rightButtonDown
		mb <- gets middleButtonDown
		if rb
		    then return [ CmdRotate dw ]
		    else if mb 
			then return [ if dw == 1 then CmdUndo else CmdRedo ]
			else do
			modify $ \s -> s { paintTileIndex = (paintTileIndex s + dw) `mod` (length paintTiles) }
			return []
		

	    drawCmd mt True = CmdPaint mt
	    drawCmd (Just t) False = CmdTile t
	    drawCmd Nothing _ = CmdWait

	    getEvents = do
		e <- waitEvent
		es <- pollEvents
		return $ e:es

	    pollEvents = do
		e <- pollEvent
		case e of
		    NoEvent -> return []
		    _ -> do
			es <- pollEvents
			return $ e:es

	    updateMousePos mode = do
		(scrCentre, size) <- getGeom
		(x,y,_) <- lift getMouseState
		let sv = (SVec (fromIntegral x) (fromIntegral y)) <+> neg scrCentre
		let mPos@(HexVec x y z) = sVec2HexVec size sv
		let (sx,sy,sz) = sVec2dHV size sv
		let isCentral = all (\(a,b) -> abs ((fromIntegral a) - b) < 0.5) $
			[(x,sx),(y,sy),(z,sz)]
		let newPos = (mPos,isCentral)
		oldPos <- gets mousePos
		when (newPos /= oldPos) $ do
		    modify $ \ds -> ds { mousePos = newPos }
		    updateHoverStr

	    updateHoverStr = do
		p@(mPos,isCentral) <- gets mousePos
		hstr <- runMaybeT $ msum
		    [ MaybeT $ (describeCommand<$>) <$> cmdAtMousePos p mode Nothing
		    , guard (mPos == uiOptPos uiOB1) >> describeUIOptionButton uiOB1
		    , guard (mPos == uiOptPos uiOB2) >> describeUIOptionButton uiOB2
		    , guard (mPos == uiOptPos uiOB3) >> describeUIOptionButton uiOB3 ]
		modify $ \ds -> ds { hoverStr = hstr }

fillArea :: HexVec -> [HexVec] -> [HexVec -> StateT MainState UIM ()] -> StateT MainState UIM () 
fillArea centre area draws = do
    selDraws <- do
	    offset <- gets listOffset
	    let na = length area
		nd = length draws
	    when (nd > (na*(offset+1))) $ lift $
		registerButtonGroup $ singleButton nextPagePos CmdNextPage 4
	    return $ drop (max 0 $ min (nd - na) (na*offset)) $ draws
    sequence_ $ map (uncurry ($)) $
	zip selDraws $ sortBy
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
	lift.renderToMain $ renderStrColAt col
	    ((if score > 0 then "+" else "") ++ show score) pos

drawNote note pos = case noteBehind note of
    Just al -> drawActiveLock al pos
    Nothing -> drawPublicNote (noteAuthor note) pos
drawActiveLock (ActiveLock name i) =
    drawNameWithChar name white (lockIndexChar i)
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

drawLockInfo :: ActiveLock -> Maybe LockInfo -> StateT MainState UIM ()
drawLockInfo al@(ActiveLock name i) Nothing = do
    let centre = hw<+>neg hv <+> 7*(i-1)<*>hu
    lift $ drawEmptyMiniLock centre
    drawNameWithCharAndCol name white (lockIndexChar i) (invisible white) centre
    lift $ registerSelectable centre 3 $ SelLockUnset al
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

    lift.renderToMain $ renderStrColAt dimWhiteCol "Accessed by:" $ accessedByPos <+> hv
    if public lockinfo
    then lift.renderToMain $ renderStrColAt pubColour "Everyone!" accessedByPos
    else if null $ accessedBy lockinfo
	then lift.renderToMain $ renderStrColAt messageCol "No-one" accessedByPos
	else fillArea accessedByPos
		[ accessedByPos <+> d | j <- [0..2], i <- [-2..3]
		    , i-j > -4, i-j < 3
		    , let d = j<*>hw <+> i<*>hu ]
		$ [ \pos -> (lift $ registerSelectable pos 0 (SelSolution note)) >> drawNote note pos
		    | note <- lockSolutions lockinfo ] ++
		[ \pos -> (lift $ registerSelectable pos 0 (SelAccessed name)) >> drawName name pos
		    | name <- accessedBy lockinfo \\ map noteAuthor (lockSolutions lockinfo) ]
    
    undecls <- gets undeclareds
    if isJust $ guard . (|| public lockinfo) . (`elem` map noteAuthor (lockSolutions lockinfo)) =<< ourName
    then lift.renderToMain $ 
	(if public lockinfo
	    then renderStrColAt pubColour "Accessed!"
	    else renderStrColAt green "Solved!")
	accessedPos
    else if any (\(Undeclared _ ls _) -> ls == lockSpec lockinfo) undecls
    then lift.renderToMain $ renderStrColAt yellow "Undeclared" accessedPos
    else do
	read <- take 3 <$> getNotesReadOn lockinfo
	unless (ourName == Just name) $ do
	    lift.renderToMain $ renderStrColAt (if length read == 3 then accessedCol else dimWhiteCol)
		"Read:" $ accessedPos <+> (-3)<*>hu
	    fillArea (accessedPos<+>neg hu) [ accessedPos <+> i<*>hu | i <- [-1..1] ]
		$ take 3 $ [ \pos -> (lift $ registerSelectable pos 0 (SelReadNote note)) >> drawName (noteAuthor note) pos
		    | note <- read ] ++ (repeat $ lift . renderToMain . drawAtRel (hollowGlyph $ dim green))

    lift.renderToMain $ renderStrColAt dimWhiteCol "Holds notes:" $ notesPos <+> hv
    if null $ notesSecured lockinfo
	then lift.renderToMain $ renderStrColAt messageCol "None" notesPos
	else fillArea notesPos
		[ notesPos <+> d | j <- [0..2], i <- [-2..3]
		    , i-j > -4, i-j < 3
		    , let d = j<*>hw <+> i<*>hu ]
		[ \pos -> (lift $ registerSelectable pos 0 (SelSecured note)) >> drawActiveLock (noteOn note) pos
		    | note <- notesSecured lockinfo ]

