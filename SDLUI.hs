{-# LANGUAGE FlexibleInstances #-}
module SDLUI (UIM) where

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Data.Char (ord)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import System.IO.Error
import Data.Word
import Data.List (elemIndex)
import Text.Show.Pretty (ppShow)
--import Debug.Trace (traceShow)

import UI
import Hex
import Command
import GameState
import Physics
import KeyBindings
import Mundanities
import SDLRender

data UIState = UIState { scrHeight::Int, scrWidth::Int
    , gsSurface::Maybe Surface
    , lastDrawArgs::Maybe DrawArgs
    , uiOptions::UIOptions
    , settingBinding::Maybe Command
    , uiKeyBindings :: Map InputMode KeyBindings
    , dispFont::Maybe TTF.Font
    , lastFrameTicks::Word32
    , paintTileIndex::Int
    , leftButtonDown::Bool, middleButtonDown::Bool, rightButtonDown::Bool
    , mousePos::HexVec
    , dispCentre::HexPos, dispLastCol::PieceColouring }
type UIM = StateT UIState IO
nullUIState = UIState 0 0 Nothing Nothing defaultUIOptions Nothing Map.empty Nothing 0 0 False False False zero (PHS zero) Map.empty

data UIOptions = UIOptions
    { useFiveColouring::Bool
    , showBlocks::Bool
    , whsButtons::WrHoSel
    }
    deriving (Eq, Ord, Show, Read)
defaultUIOptions = UIOptions True True WHSSelected
modifyUIOptions f = modify $ \s -> s { uiOptions = f $ uiOptions s }

maxlocksize = 12 -- TODO: use actual lock size?
periphery 0 = ((3*maxlocksize)`div`2)<*>hu <+> ((3*maxlocksize)`div`4)<*>hv
periphery n = rotate n $ periphery 0
messageLineStart = (maxlocksize+1)<*>hw

renderToMain :: RenderM a -> UIM a
renderToMain m = do
    surf <- liftIO getVideoSurface
    renderToMainWithSurf surf m
renderToMainWithSurf :: Surface -> RenderM a -> UIM a
renderToMainWithSurf surf m = do
    (scrCentre, size) <- getGeom
    centre <- gets dispCentre
    mfont <- gets dispFont
    liftIO $ runReaderT m $ RenderContext surf centre scrCentre size mfont


refresh :: UIM ()
refresh = do 
    surface <- liftIO getVideoSurface
    liftIO $ SDL.flip surface

waitFrame :: UIM ()
waitFrame = do
    last <- gets lastFrameTicks
    let next = last + 1000 `div` 30
    now <- liftIO getTicks
    -- liftIO $ print now
    if now < next
	then liftIO $ delay (next - now)
	else return ()
    modify $ \ds -> ds { lastFrameTicks = now }


data Button = Button { buttonPos::HexVec, buttonCmd::Command }
getButtons :: InputMode -> UIM [ ([Button],(Int,Int)) ]
getButtons mode = do
    whs <- gets $ whsButtons.uiOptions
    return $ case mode of 
	IMEdit -> [(commontl ++
		[ Button (tl<+>hv) CmdResetTools
		, Button (tl<+>(neg hw)) CmdPlay
		],(0,2))
	    , ([quitButton, Button (br<+>2<*>hu) CmdWriteState],(0,2))
	    , ( [ Button (bl<+>dir) (CmdDir whs dir) | dir <- hexDirs ], (4,0) )
	    , ([Button (paintButtonStart <+> hu <+> i<*>hv) (paintTileCmds!!i)
		| i <- take (length paintTiles) [0..] ],(5,0))
	    ]
	IMPlay -> 
	    [ (commontl,(0,2))
	    , ([quitButton],(0,2))
	    , ( [ Button bl CmdWait ], (0,0))
	    , ( [ Button (bl<+>dir) (CmdDir whs dir) | dir <- hexDirs ], (4,0) )
	    ] ++
	    (if whs == WHSWrench then [] else
		[ ( [ Button (bl<+>((-2)<*>hv)) (CmdRotate (-1))
		    , Button (bl<+>((-2)<*>hw)) (CmdRotate 1)
		    ], (4,0) )
	    ]) ++
	    (if whs /= WHSSelected then [] else
		[ ( [ Button (bl<+>(2<*>hv)<+>hw) (CmdTile $ ArmTile hw False)
		    , Button (bl<+>(2<*>hv)) (CmdTile $ WrenchTile zero)
		    ], (2,0) )
	    ])
	{- IMConfirm -> [
	    ([ quitButton
	    , Button (br<+>hw) CmdYes
	    , Button (br<+>(neg hv)) CmdNo
	    ],(0,2)) ]
	-}
	where
	    commontl = [ Button tl CmdUndo, Button (tl<+>neg hu) CmdRedo ]
	    quitButton = Button br CmdQuit
	    tl = periphery 2
	    bl = periphery 3
	    br = periphery 5


data UIOptButton a = UIOptButton { getUIOpt::UIOptions->a, setUIOpt::a->UIOptions->UIOptions,
    uiOptVals::[a], uiOptPos::HexVec, uiOptGlyph::a->Glyph }

-- non-uniform type, so can't use a list...
uiOB1 = UIOptButton useFiveColouring (\v o -> o {useFiveColouring=v}) [True,False]
	(periphery 0 <+> 2 <*> neg hw <+> neg hv) useFiveColourButton
uiOB2 = UIOptButton showBlocks (\v o -> o {showBlocks=v}) [True,False]
	(periphery 0 <+> 2 <*> neg hw <+> 3 <*> neg hv) showBlocksButton
uiOB3 = UIOptButton whsButtons (\v o -> o {whsButtons=v}) [WHSSelected, WHSWrench, WHSHook]
	(periphery 3 <+> 3 <*> hv) whsButtonsButton

drawUIOptionButtons :: InputMode -> UIM ()
drawUIOptionButtons mode = do
    drawUIOptionButton uiOB1
    drawUIOptionButton uiOB2 
    when (mode == IMPlay) $ drawUIOptionButton uiOB3
drawUIOptionButton b = do
    value <- gets $ (getUIOpt b).uiOptions
    renderToMain $ drawAtRel (uiOptGlyph b value) $ uiOptPos b
-- XXX: hand-hacking lenses...
toggleUIOption (UIOptButton getopt setopt vals _ _) = do
    value <- gets $ getopt.uiOptions
    let value' = head $ drop (1 + (fromMaybe 0 $ elemIndex value vals)) $ cycle vals
    modifyUIOptions $ setopt value'

readUIConfigFile :: UIM ()
readUIConfigFile = do 
    path <- liftIO $ confFilePath "SDLUI.conf"
    mOpts <- liftIO $ readReadFile path
    case mOpts of
	Just opts -> modify $ \s -> s {uiOptions = opts}
	Nothing -> return ()
writeUIConfigFile :: UIM ()
writeUIConfigFile = do
    path <- liftIO $ confFilePath "SDLUI.conf"
    opts <- gets uiOptions
    liftIO makeConfDir
    liftIO $ writeFile path $ ppShow opts

readBindings :: UIM ()
readBindings = do 
    path <- liftIO $ confFilePath "bindings"
    mbdgs <- liftIO $ readReadFile path
    case mbdgs of
	Just bdgs -> modify $ \s -> s {uiKeyBindings = bdgs}
	Nothing -> return ()
writeBindings :: UIM ()
writeBindings = do
    path <- liftIO $ confFilePath "bindings"
    bdgs <- gets uiKeyBindings
    liftIO makeConfDir
    liftIO $ writeFile path $ ppShow bdgs

paintTiles :: [ Maybe Tile ]
paintTiles =
    [ Just $ BlockTile []
    , Just $ SpringTile Relaxed zero
    , Just $ PivotTile zero
    , Just $ ArmTile zero False
    , Just BallTile
    , Nothing
    ]

paintTileCmds = map (maybe CmdWait CmdTile) paintTiles

getEffPaintTileIndex :: UIM Int
getEffPaintTileIndex = do
    mods <- liftIO getModState
    if (or $ map (`elem` mods) [KeyModLeftCtrl, KeyModRightCtrl]) 
    then return $ length paintTiles - 1
    else gets paintTileIndex

paintButtonStart :: HexVec
paintButtonStart = periphery 0 <+> (- length paintTiles `div` 2)<*>hv

drawPaintSel :: UIM ()
drawPaintSel = do
    pti <- getEffPaintTileIndex
    case (paintTiles!!pti) of
	Nothing -> return ()
	Just t -> renderToMain $ drawAtRel (tileGlyph t (dim $ colourWheel 5)) (periphery 0)

drawPaintButtons :: UIM ()
drawPaintButtons = do
    pti <- getEffPaintTileIndex
    renderToMain $ sequence_ [ 
	do
	    let gl = case paintTiles!!i of
		    Nothing -> hollowGlyph $ dim $ colourWheel 5
		    Just t -> tileGlyph t $ dim $ colourWheel 5
	    drawAtRel gl pos
	    when selected $ drawAtRel cursor pos
	| i <- take (length paintTiles) [0..]
	, let pos = paintButtonStart <+> i<*>hv
	, let selected = i == pti
	]

getGeom :: UIM (SVec, Int)
getGeom = do 
    h <- gets scrHeight 
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    let size = minimum [ w`div`80, floor $ (fromIntegral h) / ((sqrt 3)*25) ]
    return (scrCentre, size)


data DrawArgs = DrawArgs [PieceIdx] Bool [Alert] GameState UIOptions
    deriving (Eq, Ord, Show)

drawMainGameState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawMainGameState highlight colourFixed alerts st = do
    uiopts <- gets uiOptions
    drawMainGameState' $ DrawArgs highlight colourFixed alerts st uiopts

drawMainGameState' :: DrawArgs -> UIM ()
drawMainGameState' args@(DrawArgs highlight colourFixed alerts st uiopts) = do
    lastArgs <- gets lastDrawArgs
    void $ if lastArgs == Just args
	then do
	    vidSurf <- liftIO getVideoSurface
	    gsSurf <- liftM fromJust $ gets gsSurface
	    liftIO $ blitSurface gsSurf Nothing vidSurf Nothing
	else do
	    modify $ \ds -> ds { lastDrawArgs = Just args }
	    let board = stateBoard st
	    lastCol <- gets dispLastCol
	    let coloured = colouredPieces colourFixed st
	    let colouring = if useFiveColouring uiopts
		then boardColouring st coloured lastCol
		else pieceTypeColouring st coloured
	    modify $ \ds -> ds { dispLastCol = colouring }
	    gsSurf <- liftM fromJust $ gets gsSurface
	    renderToMainWithSurf gsSurf $ do
		erase
		sequence_ [ drawAt glyph pos | 
		    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring highlight) board
		    ]

		when (showBlocks uiopts) $ sequence_
		    $ [ drawBlocked st colouring False force |
		    AlertBlockedForce force <- alerts ]
		    ++ [ drawBlocked st colouring True force |
		    AlertBlockingForce force <- alerts ]
		    ++ [ drawBlocked st colouring True force |
		    AlertResistedForce force <- alerts ]
		    -- ++ [ drawAt collisionMarker pos | AlertCollision pos <- alerts ]
	    vidSurf <- liftIO getVideoSurface
	    liftIO $ blitSurface gsSurf Nothing vidSurf Nothing

getBindingStr :: InputMode -> UIM (Command -> String)
getBindingStr mode = do
    setting <- gets settingBinding
    uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
    return (\cmd ->
	if Just cmd == setting then ".."
	else findBinding (uibdgs ++ bindings mode) cmd)

drawButtons :: InputMode -> UIM ()
drawButtons mode = do
    buttons <- getButtons mode
    bindingStr <- getBindingStr mode
    renderToMain $ sequence_ $ concat [
	    [ drawAtRel (buttonGlyph col) v >> renderStrAt bdg v |
		(i,(v,bdg)) <- enumerate $ map (\b->(buttonPos b, bindingStr $ buttonCmd b)) $ buttonGroup
		, let col = dim $ colourWheel (base+inc*i) ]
	| (buttonGroup,(base,inc)) <- buttons
	]
    where enumerate = zip [0..]

initVideo :: Int -> Int -> UIM ()
initVideo w h = do
    liftIO $ setVideoMode w h 0 [Resizable]
    modify $ \ds -> ds { scrWidth = w }
    modify $ \ds -> ds { scrHeight = h }
    surf <- liftIO $ createRGBSurface [ HWSurface ] w h 16 0 0 0 0
    modify $ \ds -> ds { gsSurface = Just surf, lastDrawArgs = Nothing }
    (_,size) <- getGeom
    font <- liftIO $ TTF.tryOpenFont "VeraMono.ttf" (size)
    modify $ \ds -> ds { dispFont = font }

say str = renderToMain $ renderStrAt str $ messageLineStart

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawMainState = get >>= \s -> case s of
	PlayState st _ alerts wsel solved _ _ -> lift $ do
	    waitFrame
	    let selTools = [ idx |
		    (idx, PlacedPiece pos p) <- enumVec $ placedPieces st
		    , or [wsel && isWrench p, not wsel && isHook p] ]
	    drawMainGameState selTools False alerts st
	    drawButtons IMPlay
	    drawUIOptionButtons IMPlay
	    when solved $ say "Unlocked!"
	    refresh

	EditState (st:_) _ _ _ selPiece selPos _ -> lift $ do
	    waitFrame
	    drawMainGameState (maybeToList selPiece) True [] st
	    renderToMain $ drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	    -- drawPaintSel
	    drawButtons IMEdit
	    drawPaintButtons
	    drawUIOptionButtons IMEdit
	    refresh

	_ -> return ()

    drawMessage prompt = do
	say prompt
	refresh
    drawError prompt = do
	-- TODO
	say prompt
	refresh

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert _ = return ()

    initUI = liftM isJust (runMaybeT $ do
	catchIOErrorMT $ SDL.init [InitVideo]
	catchIOErrorMT $ TTF.init
	lift $ do
	    modes <- liftIO $ listModes Nothing [Resizable]
	    let Rect _ _ w h = case modes of
		    Modes (mode:_) -> mode
		    AnyOK -> Rect 0 0 1024 768
		    NonAvailable -> error "SDL says no resolution available"
	    initVideo w h
	    liftIO $ warpMouse (fromIntegral $ w`div`2) (fromIntegral $ h`div`2)
	    renderToMain $ erase
	    liftIO $ enableUnicode True
	    liftIO $ enableKeyRepeat 500 30
	    readUIConfigFile
	    readBindings
	)
	where
	    catchIOErrorMT m = MaybeT $ liftIO $ catchIOError (m >> return (Just ())) (\_ -> return Nothing)

    endUI = do
	writeUIConfigFile
	writeBindings
	liftIO $ quit

    unblockInput = return $ pushEvent VideoExpose

    getInput mode = do
	events <- liftIO $ getEvents
	concat `liftM` (sequence $ map processEvent events)
	where
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
				, (pt == Nothing && mCmd == Just CmdWait) ||
				    (isJust $ do
					pt' <- pt
					CmdTile t <- mCmd
					guard $ sameTileType t pt') ]
			return $ maybeToList mCmd
	    processEvent (MouseMotion _ _ _ _) = case mode of
		IMEdit -> do
		    (mPos,_) <- getMousePos
		    centre <- gets dispCentre
		    relMoves <- mouseTo mPos
		    return $ relMoves ++ [ CmdMoveTo $ mPos <+> centre ]
		    where
			mouseTo mPos = do
			    oldMPos <- gets mousePos
			    let dir = hexVec2HexDirOrZero $ mPos <-> oldMPos
			    if dir /= zero
				then do
				    modify $ \s -> s {mousePos = oldMPos <+> dir}
				    centre <- gets dispCentre
				    pti <- getEffPaintTileIndex
				    lb <- gets leftButtonDown
				    more <- mouseTo mPos
				    return $ [ CmdDir WHSSelected dir ] ++
					(if lb then [ drawCmd (paintTiles!!pti) True ] else []) ++
					more
				else return []
		_ -> return []
	    processEvent (MouseButtonDown _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = True }
		(mPos,central) <- getMousePos
		buttons <- (concat . map fst) `liftM` getButtons mode
		rb <- gets rightButtonDown
		let hotspotAction = listToMaybe
			$ [ return [buttonCmd button]
			    | button <- buttons, mPos == buttonPos button, central]
			++ [ (modify $ \s -> s {paintTileIndex = i}) >> return []
			    | i <- take (length paintTiles) [0..]
			    , mPos == paintButtonStart <+> i<*>hv ]
			++ [ toggleUIOption uiOB1 >> return []
			    | mPos == uiOptPos uiOB1 ]
			++ [ toggleUIOption uiOB2 >> return []
			    | mPos == uiOptPos uiOB2 ]
			++ [ toggleUIOption uiOB3 >> return []
			    | mPos == uiOptPos uiOB3 ]
		if rb
		then return [ CmdWait ]
		else case hotspotAction of
		    Just a -> a
		    Nothing -> do
			pti <- getEffPaintTileIndex
			return $ [ drawCmd (paintTiles!!pti) False ]
	    processEvent (MouseButtonUp _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = False }
		return []
	    processEvent (MouseButtonDown _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = True }
		(mPos,central) <- getMousePos
		buttons <- (concat . map fst) `liftM` getButtons mode
		case listToMaybe [ buttonCmd button
			| button <- buttons, mPos == buttonPos button, central] of
		    Just cmd -> do
			modify $ \s -> s { settingBinding = Just cmd }
			return []
		    Nothing -> return [ CmdSelect ]
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
	    processEvent (VideoResize w h) =
		if mode == IMTextInput
		    then -- TODO: resize only when we next draw the screen
			return []
		    else do
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
		

	    drawCmd (Just t) True = CmdPaint t
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

	    getMousePos = do
		(scrCentre, size) <- getGeom
		(x,y,_) <- lift getMouseState
		let sv = (SVec (fromIntegral x) (fromIntegral y)) <+> neg scrCentre
		let mPos@(HexVec x y z) = sVec2HexVec size sv
		let (sx,sy,sz) = sVec2dHV size sv
		let isCentral = and $ map (\(a,b) -> abs ((fromIntegral a) - b) < 0.5) $
			[(x,sx),(y,sy),(z,sz)]
		return (mPos, isCentral)
