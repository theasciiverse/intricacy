-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module SDLUI where

import Graphics.UI.SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Control.Concurrent.STM
import Control.Applicative hiding ((<*>))
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Data.Word
import Data.Array
import Data.List
import Data.Ratio
import Data.Function (on)
--import Debug.Trace (traceShow)

import Hex
import Command
import GameState (stateBoard)
import GameStateTypes
import BoardColouring
import Lock
import Physics
import KeyBindings
import Mundanities
import Metagame
import SDLRender
import InputMode
import Maxlocksize

data UIState = UIState { scrHeight::Int, scrWidth::Int
    , gsSurface::Maybe Surface
    , bgSurface::Maybe Surface
    , lastDrawArgs::Maybe DrawArgs
    , miniLocks::Map Lock Surface
    , metaSelectables::Map HexVec Selectable
    , contextButtons::[ButtonGroup]
    , uiOptions::UIOptions
    , settingBinding::Maybe Command
    , uiKeyBindings :: Map InputMode KeyBindings
    , dispFont::Maybe TTF.Font
    , dispFontSmall::Maybe TTF.Font
    , lastFrameTicks::Word32
    , paintTileIndex::Int
    , leftButtonDown::Maybe HexVec, middleButtonDown::Maybe HexVec, rightButtonDown::Maybe HexVec
    , mousePos::(HexVec,Bool)
    , message::Maybe (Pixel, String)
    , hoverStr :: Maybe String
    , dispCentre::HexPos
    , dispLastCol::PieceColouring }
    deriving (Eq, Ord, Show)
type UIM = StateT UIState IO
nullUIState = UIState 0 0 Nothing Nothing Nothing Map.empty Map.empty []
    defaultUIOptions Nothing Map.empty Nothing Nothing 0 0 Nothing Nothing Nothing
    (zero,False) Nothing Nothing (PHS zero) Map.empty

data UIOptions = UIOptions
    { useFiveColouring::Bool
    , showBlocks::Bool
    , whsButtons::WrHoSel
    , useBackground::Bool
    }
    deriving (Eq, Ord, Show, Read)
defaultUIOptions = UIOptions False False WHSSelected True
modifyUIOptions f = modify $ \s -> s { uiOptions = f $ uiOptions s }

renderToMain :: RenderM a -> UIM a
renderToMain m = do
    surf <- liftIO getVideoSurface
    renderToMainWithSurf surf m
renderToMainWithSurf :: Surface -> RenderM a -> UIM a
renderToMainWithSurf surf m = do
    (scrCentre, size) <- getGeom
    centre <- gets dispCentre
    mfont <- gets dispFont
    bgsurf <- gets bgSurface
    liftIO $ runReaderT m $ RenderContext surf bgsurf centre scrCentre size mfont


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
    when (now < next) $
	liftIO $ delay (next - now)
    modify $ \ds -> ds { lastFrameTicks = now }


data Button = Button { buttonPos::HexVec, buttonCmd::Command }
    deriving (Eq, Ord, Show)
type ButtonGroup = ([Button],(Int,Int))
singleButton :: HexVec -> Command -> Int -> ButtonGroup
singleButton pos cmd col = ([Button pos cmd], (col,0))
getButtons :: InputMode -> UIM [ ButtonGroup ]
getButtons mode = do
    whs <- gets $ whsButtons.uiOptions
    cntxtButtons <- gets contextButtons
    return $ cntxtButtons ++ global ++ case mode of 
	IMEdit -> [
	    singleButton (tl<+>hv) CmdTest 4
	    , singleButton (tl<+>(neg hw)) CmdPlay 2
	    , markGroup
	    , singleButton (br<+>2<*>hu) CmdWriteState 2
	    , ( [ Button (bl<+>dir) (CmdDir WHSSelected dir) | dir <- hexDirs ] ++
		[ Button (bl<+>((-2)<*>hv)) (CmdRotate WHSSelected (-1))
		, Button (bl<+>((-2)<*>hw)) (CmdRotate WHSSelected 1) ], (4,0) )
	    , ( [ Button bl CmdSelect ], (0,0))
	    , ([Button (paintButtonStart <+> hu <+> i<*>hv) (paintTileCmds!!i)
		| i <- take (length paintTiles) [0..] ],(5,0))
	    ]
	IMPlay -> 
	    [ markGroup
	    , ( [ Button bl CmdWait ], (0,0))
	    , ( [ Button (bl<+>dir) (CmdDir whs dir) | dir <- hexDirs ], (4,0) )
	    ] ++
	    (if whs == WHSWrench then [] else
		[ ( [ Button (bl<+>((-2)<*>hv)) (CmdRotate whs (-1))
		    , Button (bl<+>((-2)<*>hw)) (CmdRotate whs 1)
		    ], (4,0) )
	    ]) ++
	    (if whs /= WHSSelected then [] else
		[ ( [ Button (bl<+>(2<*>hv)<+>hw) (CmdTile $ HookTile)
		    , Button (bl<+>(2<*>hv)) (CmdTile $ WrenchTile zero)
		    ], (2,0) )
	    ]) ++
	    [ singleButton tr CmdOpen 1 ]
	IMReplay -> [ markGroup ]
	IMMeta ->
	    [ singleButton serverPos CmdSetServer 2
	    , singleButton (serverPos<+>neg hu) CmdToggleCacheOnly 0
	    , singleButton lockLinePos CmdSelectLock 4
	    , singleButton (miniLockPos <+> 2<*>hv <+> neg hu <+> hw) CmdEdit 2
	    , singleButton (codenamePos <+> neg hu) (CmdSelCodename Nothing) 2
	    , singleButton (serverPos <+> 2<*>neg hv <+> hw) CmdTutorials 1
	    ]
		
	_ -> []
	where
	    markGroup = ([Button (tl<+>hw) CmdMark, Button (tl<+>hw<+>neg hu) CmdJumpMark],(0,0))
	    global = if mode == IMTextInput then [] else
		[ singleButton br CmdQuit 0
		, singleButton (tr <+> 3<*>hv <+> 3<*>hu) CmdHelp 3 ]
	    tr = periphery 0
	    tl = periphery 2
	    bl = periphery 3
	    br = periphery 5

data Selectable = SelOurLock
    | SelLock ActiveLock
    | SelLockUnset ActiveLock
    | SelSelectedCodeName
    | SelOurAL
    | SelUndeclared Undeclared
    | SelReadNote NoteInfo
    | SelSolution NoteInfo
    | SelAccessed Codename
    | SelRandom Codename
    | SelSecured NoteInfo
    | SelOldLock LockSpec
    deriving (Eq, Ord, Show)

registerSelectable v r s =
    modify $ \ds -> ds {metaSelectables = foldr
	(`Map.insert` s) (metaSelectables ds) $ map (v<+>) $ hexDisc r}
registerButtonGroup g = modify $ \ds -> ds {contextButtons = g:contextButtons ds}
registerButton pos cmd col = registerButtonGroup $ singleButton pos cmd col
clearSelectables,clearButtons :: UIM ()
clearSelectables = modify $ \ds -> ds {metaSelectables = Map.empty}
clearButtons = modify $ \ds -> ds {contextButtons = []}

registerUndoButtons noUndo noRedo = do
    unless noUndo $ registerButton (periphery 2) CmdUndo 0
    unless noRedo $ registerButton (periphery 2<+>neg hu) CmdRedo 2

commandOfSelectable IMMeta SelOurLock _ = CmdEdit
commandOfSelectable IMMeta (SelLock (ActiveLock _ i)) False = CmdSolve (Just i)
commandOfSelectable IMMeta (SelLock (ActiveLock _ i)) True = CmdPlaceLock (Just i)
commandOfSelectable IMMeta (SelLockUnset (ActiveLock _ i)) _ = CmdPlaceLock (Just i)
commandOfSelectable IMMeta SelSelectedCodeName False = CmdSelCodename Nothing
commandOfSelectable IMMeta SelSelectedCodeName True = CmdHome
commandOfSelectable IMMeta SelOurAL _ = CmdHome
commandOfSelectable IMMeta (SelUndeclared undecl) _ = CmdDeclare $ Just undecl
commandOfSelectable IMMeta (SelReadNote note) False = CmdSelCodename $ Just $ noteAuthor note
commandOfSelectable IMMeta (SelReadNote note) True = CmdViewSolution $ Just note
commandOfSelectable IMMeta (SelSolution note) False = CmdSelCodename $ Just $ noteAuthor note
commandOfSelectable IMMeta (SelSolution note) True = CmdViewSolution $ Just note
commandOfSelectable IMMeta (SelAccessed name) _ = CmdSelCodename $ Just name
commandOfSelectable IMMeta (SelRandom name) _ = CmdSelCodename $ Just name
commandOfSelectable IMMeta (SelSecured note) False = CmdSelCodename $ Just $ lockOwner $ noteOn note
commandOfSelectable IMMeta (SelSecured note) True = CmdViewSolution $ Just note
commandOfSelectable IMMeta (SelOldLock ls) _ = CmdPlayLockSpec $ Just ls
commandOfSelectable IMTextInput (SelLock (ActiveLock _ i)) _ = CmdInputSelLock i
commandOfSelectable IMTextInput (SelLockUnset (ActiveLock _ i)) _ = CmdInputSelLock i
commandOfSelectable IMTextInput (SelReadNote note) _ = CmdInputCodename $ noteAuthor note
commandOfSelectable IMTextInput (SelSolution note) _ = CmdInputCodename $ noteAuthor note
commandOfSelectable IMTextInput (SelSecured note) _ = CmdInputCodename $ lockOwner $ noteOn note
commandOfSelectable IMTextInput (SelRandom name) _ = CmdInputCodename name
commandOfSelectable IMTextInput (SelUndeclared undecl) _ = CmdInputSelUndecl undecl
commandOfSelectable _ _ _ = CmdNone

cmdAtMousePos pos@(mPos,central) im selMode = do
    buttons <- (concat . map fst) <$> getButtons im
    sels <- gets metaSelectables
    return $ listToMaybe $
	[ buttonCmd button
	    | button <- buttons, mPos == buttonPos button, central]
	++ maybe [] (\isRight -> [ commandOfSelectable im sel isRight
	    | Just sel <- [Map.lookup mPos sels] ]) selMode

data UIOptButton a = UIOptButton { getUIOpt::UIOptions->a, setUIOpt::a->UIOptions->UIOptions,
    uiOptVals::[a], uiOptPos::HexVec, uiOptGlyph::a->Glyph, uiOptDescr::a->String }

-- non-uniform type, so can't use a list...
uiOB1 = UIOptButton useFiveColouring (\v o -> o {useFiveColouring=v}) [True,False]
	(periphery 0 <+> 2 <*> neg hw <+> neg hv) useFiveColourButton
	(\v -> if v then "Adjacent pieces get different colours" else
	"Pieces are coloured according to type")
uiOB2 = UIOptButton showBlocks (\v o -> o {showBlocks=v}) [True,False]
	(periphery 0 <+> 2 <*> neg hw <+> 3 <*> neg hv) showBlocksButton
	(\v -> if v then "Blocked and blocking forces are annotated" else
	"Blockage annotations disabled")
uiOB3 = UIOptButton whsButtons (\v o -> o {whsButtons=v}) [WHSSelected, WHSWrench, WHSHook]
	(periphery 3 <+> 3 <*> hv) whsButtonsButton
	(\v -> "Showing buttons for controlling " ++ case v of
	    WHSSelected -> "selected tool"
	    WHSWrench -> "wrench"
	    WHSHook -> "hook")

drawUIOptionButtons :: InputMode -> UIM ()
drawUIOptionButtons mode = when (mode `elem` [IMPlay, IMEdit]) $ do
    drawUIOptionButton uiOB1
    drawUIOptionButton uiOB2 
    when (mode == IMPlay) $ drawUIOptionButton uiOB3
drawUIOptionButton b = do
    value <- gets $ (getUIOpt b).uiOptions
    renderToMain $ mapM_ (\g -> drawAtRel g (uiOptPos b))
	[hollowGlyph $ obscure purple, uiOptGlyph b value]
describeUIOptionButton b = do
    value <- gets $ (getUIOpt b).uiOptions
    return $ uiOptDescr b value
-- XXX: hand-hacking lenses...
toggleUIOption (UIOptButton getopt setopt vals _ _ _) = do
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
    liftIO $ writeFile path $ show opts

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
    liftIO $ writeFile path $ show bdgs

getBindings mode = do
    uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
    return $ uibdgs ++ bindings mode

paintTiles :: [ Maybe Tile ]
paintTiles =
    [ Just BallTile
    , Just $ ArmTile zero False
    , Just $ PivotTile zero
    , Just $ SpringTile Relaxed zero
    , Just $ BlockTile []
    , Nothing
    ]

paintTileCmds = map (maybe CmdDelete CmdTile) paintTiles

getEffPaintTileIndex :: UIM Int
getEffPaintTileIndex = do
    mods <- liftIO getModState
    if any (`elem` mods) [KeyModLeftCtrl, KeyModRightCtrl]
    then return $ length paintTiles - 1
    else gets paintTileIndex

paintButtonStart :: HexVec
paintButtonStart = periphery 0 <+> (- length paintTiles `div` 2)<*>hv

drawPaintButtons :: UIM ()
drawPaintButtons = do
    pti <- getEffPaintTileIndex
    renderToMain $ sequence_ [ 
	do
	    let gl = case paintTiles!!i of
		    Nothing -> hollowInnerGlyph $ dim purple
		    Just t -> tileGlyph t $ dim purple
	    drawAtRel gl pos
	    when selected $ drawAtRel cursor pos
	| i <- take (length paintTiles) [0..]
	, let pos = paintButtonStart <+> i<*>hv
	, let selected = i == pti
	]

periphery 0 = ((3*maxlocksize)`div`2)<*>hu <+> ((3*maxlocksize)`div`4)<*>hv
periphery n = rotate n $ periphery 0
-- ^ XXX only peripheries 0,2,3,5 are guaranteed to be on-screen!
--messageLineStart = (maxlocksize+1)<*>hw
messageLineCentre = ((maxlocksize+1)`div`2)<*>hw <+> ((maxlocksize+1+1)`div`2)<*>neg hv
titlePos = (maxlocksize+1)<*>hv <+> ((maxlocksize+1)`div`2)<*>hu

screenWidthHexes,screenHeightHexes::Int
screenWidthHexes = 32
screenHeightHexes = 25
getGeom :: UIM (SVec, Int)
getGeom = do 
    h <- gets scrHeight 
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    -- |size is the greatest integer such that
    -- and [2*size*screenWidthHexes <= width
    --	, 3*ysize size*screenHeightHexes <= height]
    --	where ysize size = round $ fromIntegral size / sqrt 3
    let size = max 1 $ minimum [ w`div`(2*screenWidthHexes)
	    , floor $ sqrt 3 * (0.5 + (fromIntegral $ h`div`(3*screenHeightHexes)))]
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

drawMiniLock :: Lock -> HexVec -> UIM ()
drawMiniLock lock v = do
    surface <- Map.lookup lock <$> gets miniLocks >>= maybe new return
    renderToMain $ blitAt surface v
    where
	miniLocksize = 3
	new = do
	    (_, size) <- getGeom
	    let minisize = size `div` (ceiling $ lockSize lock % miniLocksize)
	    let width = size*2*(miniLocksize*2+1)
	    let height = ceiling $ fromIntegral size * sqrt 3 * fromIntegral (miniLocksize*2+1+1)
	    surf <- liftIO $ createRGBSurface [] width height 16 0 0 0 0
	    liftIO $ setColorKey surf [SrcColorKey] $ Pixel 0
	    uiopts <- gets uiOptions
	    let st = snd $ reframe lock
		coloured = colouredPieces False st
		colouring = if useFiveColouring uiopts
		    then boardColouring st coloured Map.empty
		    else pieceTypeColouring st coloured
		draw = sequence_ [ drawAt glyph pos | 
		    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring []) $ stateBoard st ]
	    liftIO $ runReaderT draw $
		RenderContext surf Nothing (PHS zero) (SVec (width`div`2) (height`div`2)) minisize Nothing
	    clearOldMiniLocks
	    modify $ \ds -> ds { miniLocks = Map.insert lock surf $ miniLocks ds }
	    return surf

-- | TODO: do this more cleverly
clearOldMiniLocks =
    (>=50).Map.size <$> gets miniLocks >>= flip when clearMiniLocks
clearMiniLocks = modify $ \ds -> ds { miniLocks = Map.empty}

drawEmptyMiniLock v =
    renderToMain $ recentreAt v $ rescaleRender 6 $ drawAtRel (hollowInnerGlyph $ dim white) zero

getBindingStr :: InputMode -> UIM (Command -> String)
getBindingStr mode = do
    setting <- gets settingBinding
    uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
    return (\cmd ->
	if Just cmd == setting then "??"
	else maybe "" showKey $ findBinding (uibdgs ++ bindings mode) cmd)

drawButtons :: InputMode -> UIM ()
drawButtons mode = do
    buttons <- getButtons mode
    bindingStr <- getBindingStr mode
    renderToMain $ sequence_ $ concat [
	    [ drawAtRel (buttonGlyph col) v >> renderStrColAt buttonTextCol bdg v |
		(i,(v,bdg)) <- enumerate $ map (\b->(buttonPos b, bindingStr $ buttonCmd b)) $ buttonGroup
		, let col = dim $ colourWheel (base+inc*i) ]
	| (buttonGroup,(base,inc)) <- buttons
	]
    where enumerate = zip [0..]

initVideo :: Int -> Int -> UIM ()
initVideo w h = do
    liftIO $ setVideoMode w h 0 [Resizable]
    
    -- see what size we actually got:
    vinfo <- liftIO $ getVideoInfo
    let [w',h'] = map ($vinfo) [videoInfoWidth,videoInfoHeight]

    modify $ \ds -> ds { scrWidth = w' }
    modify $ \ds -> ds { scrHeight = h' }
    gssurf <- liftIO $ createRGBSurface [] w' h' 16 0 0 0 0
    modify $ \ds -> ds { gsSurface = Just gssurf, lastDrawArgs = Nothing }

    (_,size) <- getGeom
    let fontfn = "VeraMono.ttf"
    fontpath <- liftIO $ getDataPath fontfn
    font <- liftIO $ TTF.tryOpenFont fontpath size
    smallFont <- liftIO $ TTF.tryOpenFont fontpath (2*size`div`3)
    modify $ \ds -> ds { dispFont = font, dispFontSmall = smallFont }

    useBG <- gets $ useBackground.uiOptions
    mbg <- if useBG then do
	    bgsurf <- liftIO $ createRGBSurface [] w' h' 16 0 0 0 0
	    renderToMainWithSurf bgsurf $ drawBasicBG $ 2*(max screenWidthHexes screenHeightHexes)`div`3
	    return $ Just bgsurf
	else return Nothing
    modify $ \ds -> ds { bgSurface = mbg }

    clearMiniLocks

    when (isNothing font) $ lift $ do
	let text = "Warning: font file not found at "++fontpath++".\n"
	putStr text
	writeFile "error.log" text 

drawMsgLine = void.runMaybeT $ do
    (col,str) <- msum
	[ ((,) dimWhiteCol) <$> MaybeT (gets hoverStr)
	, MaybeT $ gets message
	]
    lift $ do
	renderToMain $ blankRow messageLineCentre
	smallFont <- gets dispFontSmall
	renderToMain $
	    (if length str > screenWidthHexes * 3 then withFont smallFont else id) $
		renderStrColAt col str messageLineCentre

setMsgLineNoRefresh col str = do
    modify $ \s -> s { message = Just (col,str) }
    unless (null str) $ modify $ \s -> s { hoverStr = Nothing }
    drawMsgLine 
setMsgLine col str = setMsgLineNoRefresh col str >> refresh

drawTitle (Just title) = renderToMain $ renderStrColAt messageCol title titlePos
drawTitle Nothing = return ()
    
say = setMsgLine messageCol
sayError = setMsgLine errorCol

miniLockPos = (-9)<*>hw <+> hu
lockLinePos = 2<*>(hu <+> neg hw) <+> miniLockPos
serverPos = 12<*>hv <+> 6<*>neg hu
serverWaitPos = serverPos <+> hw <+> neg hu
nextPagePos = serverPos <+> 4<*>neg hv
randomNamesPos = 9<*>hv <+> neg hu
codenamePos = (-4)<*>hw <+> 4<*>hv
undeclsPos = hv <+> neg hw <+> codenamePos
accessedOursPos = 2<*>hw <+> codenamePos
locksPos = hw<+>neg hv
retiredPos = locksPos <+> 10<*>hu <+> (-4)<*>hv
