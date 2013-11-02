{-# LANGUAGE FlexibleInstances #-}
module SDLUI (UIM) where

import Graphics.UI.SDL hiding (flip)
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Control.Concurrent.STM
import Control.Applicative hiding ((<*>))
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
import Data.Array
import Data.List
import Data.Ratio
import Text.Show.Pretty (ppShow)
--import Debug.Trace (traceShow)

import UI
import Hex
import Command
import GameState
import Lock
import Physics
import KeyBindings
import Mundanities
import Metagame
import Protocol
import Cache
import Database
import ServerAddr
import SDLRender

data UIState = UIState { scrHeight::Int, scrWidth::Int
    , gsSurface::Maybe Surface
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
    , leftButtonDown::Bool, middleButtonDown::Bool, rightButtonDown::Bool
    , mousePos::(HexVec,Bool)
    , message::Maybe (Color, String)
    , hoverStr :: Maybe String
    , dispCentre::HexPos
    , dispLastCol::PieceColouring }
type UIM = StateT UIState IO
nullUIState = UIState 0 0 Nothing Nothing Map.empty Map.empty []
    defaultUIOptions Nothing Map.empty Nothing Nothing 0 0 False False False
    (zero,False) Nothing Nothing (PHS zero) Map.empty

data UIOptions = UIOptions
    { useFiveColouring::Bool
    , showBlocks::Bool
    , whsButtons::WrHoSel
    }
    deriving (Eq, Ord, Show, Read)
defaultUIOptions = UIOptions False False WHSSelected
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
type ButtonGroup = ([Button],(Int,Int))
singleButton :: HexVec -> Command -> Int -> ButtonGroup
singleButton pos cmd col = ([Button pos cmd], (col,0))
getButtons :: InputMode -> UIM [ ButtonGroup ]
getButtons mode = do
    whs <- gets $ whsButtons.uiOptions
    cntxtButtons <- gets contextButtons
    return $ cntxtButtons ++ case mode of 
	IMEdit -> [
	    singleButton (tl<+>hv) CmdResetTools 4
	    , singleButton (tl<+>(neg hw)) CmdPlay 2
	    , ([quitButton, Button (br<+>2<*>hu) CmdWriteState],(0,2))
	    , ( [ Button (bl<+>dir) (CmdDir WHSSelected dir) | dir <- hexDirs ] ++
		[ Button (bl<+>((-2)<*>hv)) (CmdRotate (-1))
		, Button (bl<+>((-2)<*>hw)) (CmdRotate 1) ], (4,0) )
	    , ([Button (paintButtonStart <+> hu <+> i<*>hv) (paintTileCmds!!i)
		| i <- take (length paintTiles) [0..] ],(5,0))
	    ]
	IMPlay -> 
	    [ ([quitButton],(0,2))
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
	    ]) ++
	    [ singleButton tr CmdOpen 1 ]
	IMMeta ->
	    [ singleButton serverPos CmdSetServer 0
	    , singleButton lockLinePos CmdSelectLock 4
	    , singleButton (miniLockPos <+> 2<*>hv <+> neg hu <+> hw) CmdEdit 2
	    , singleButton (miniLockPos <+> 2<*>neg hv <+> hu) CmdPrevLock 4
	    , singleButton (miniLockPos <+> 2<*>neg hv <+> hu <+> neg hw) CmdNextLock 4
	    , singleButton (codenamePos <+> neg hu) (CmdSelCodename Nothing) 2
	    , singleButton (serverPos <+> 2<*>neg hv <+> hw) CmdTutorials 1
	    ]
		
	_ -> []
	where
	    quitButton = Button br CmdQuit
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

registerSelectable v r s =
    modify $ \ds -> ds {metaSelectables = foldr
	(\k -> Map.insert k s) (metaSelectables ds) $ map (v<+>) $ hexDisc r}
clearSelectables = modify $ \ds -> ds {metaSelectables = Map.empty}
registerButtonGroup g = modify $ \ds -> ds {contextButtons = g:contextButtons ds}
clearButtons = modify $ \ds -> ds {contextButtons = []}

registerUndoButtons noUndo noRedo = do
    unless noUndo $ registerButtonGroup $ singleButton (periphery 2) CmdUndo 0
    unless noRedo $ registerButtonGroup $ singleButton (periphery 2<+>neg hu) CmdRedo 2

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
    uiOptVals::[a], uiOptPos::HexVec, uiOptGlyph::a->Glyph }

-- non-uniform type, so can't use a list...
uiOB1 = UIOptButton useFiveColouring (\v o -> o {useFiveColouring=v}) [True,False]
	(periphery 0 <+> 2 <*> neg hw <+> neg hv) useFiveColourButton
uiOB2 = UIOptButton showBlocks (\v o -> o {showBlocks=v}) [True,False]
	(periphery 0 <+> 2 <*> neg hw <+> 3 <*> neg hv) showBlocksButton
uiOB3 = UIOptButton whsButtons (\v o -> o {whsButtons=v}) [WHSSelected, WHSWrench, WHSHook]
	(periphery 3 <+> 3 <*> hv) whsButtonsButton

drawUIOptionButtons :: InputMode -> UIM ()
drawUIOptionButtons mode = when (mode `elem` [IMPlay, IMEdit]) $ do
    drawUIOptionButton uiOB1
    drawUIOptionButton uiOB2 
    when (mode == IMPlay) $ drawUIOptionButton uiOB3
drawUIOptionButton b = do
    value <- gets $ (getUIOpt b).uiOptions
    renderToMain $ mapM_ (\g -> drawAtRel g (uiOptPos b))
	[hollowGlyph $ obscure $ colourWheel 5, uiOptGlyph b value]
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
    [ Just BallTile
    , Just $ ArmTile zero False
    , Just $ PivotTile zero
    , Just $ SpringTile Relaxed zero
    , Just $ BlockTile []
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
		    Nothing -> hollowInnerGlyph $ dim $ colourWheel 5
		    Just t -> tileGlyph t $ dim $ colourWheel 5
	    drawAtRel gl pos
	    when selected $ drawAtRel cursor pos
	| i <- take (length paintTiles) [0..]
	, let pos = paintButtonStart <+> i<*>hv
	, let selected = i == pti
	]

periphery 0 = ((3*maxlocksize)`div`2)<*>hu <+> ((3*maxlocksize)`div`4)<*>hv
periphery n = rotate n $ periphery 0
-- ^ XXX only peripheries 0,2,3,5 are guaranteed to be on-screen!
messageLineStart = (maxlocksize+1)<*>hw
titlePos = (maxlocksize+1)<*>hv <+> ((maxlocksize+1)`div`2)<*>hu

getGeom :: UIM (SVec, Int)
getGeom = do 
    h <- gets scrHeight 
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    let size = minimum [ w`div`(2*30), floor $ (fromIntegral h) / ((sqrt 3) * 25) ]
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
	    uiopts <- gets uiOptions
	    let st = snd $ reframe $ lock
		coloured = colouredPieces False st
		colouring = if useFiveColouring uiopts
		    then boardColouring st coloured Map.empty
		    else pieceTypeColouring st coloured
		draw = sequence_ [ drawAt glyph pos | 
		    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring []) $ stateBoard st ]
	    liftIO $ runReaderT draw $
		RenderContext surf (PHS zero) (SVec (width`div`2) (height`div`2)) minisize Nothing
	    clearOldMiniLocks
	    modify $ \ds -> ds { miniLocks = Map.insert lock surf $ miniLocks ds }
	    return surf

-- | TODO: do this more cleverly
clearOldMiniLocks =
    (>=50).Map.size <$> gets miniLocks >>= flip when clearMiniLocks
clearMiniLocks = modify $ \ds -> ds { miniLocks = Map.empty}

drawEmptyMiniLock v = do
    renderToMain $ recentreAt v $ rescaleRender 6 $ drawAtRel (hollowInnerGlyph $ dim white) zero

fillArea centre area draws =
    mapM_ (\(draw,pos) -> draw pos) $
	zip draws $ sortBy
		(\p p' -> hexLen (p<->centre) `compare` hexLen (p'<->centre))
		area

pubColour = colourWheel 5
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
drawName name pos = do
    col <- nameCol name
    lift.renderToMain $ do 
	drawAtRel (playerGlyph col) pos
	renderStrColAt messageCol name pos
drawRelScore name pos = do
    col <- nameCol name
    relScore <- getRelScore name
    flip (maybe (return ())) relScore $ \score ->
	lift.renderToMain $ renderStrColAt (pixelToColor col)
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
    let up = SVec 0 $ - floor (size * sqrt 3 / 6)
    let down = SVec 0 $ ceiling $ size * sqrt 3 / 3
    smallFont <- lift $ gets dispFontSmall
    lift.renderToMain $ do
	drawAtRel (playerGlyph col) pos
	displaceRender up $ 
	    renderStrColAt messageCol name pos
	displaceRender down $ withFont smallFont $
	    renderStrColAt (pixelToColor charcol) [char] pos

drawLockInfo :: ActiveLock -> Maybe LockInfo -> Maybe (TVar (Maybe Lock)) -> StateT MainState UIM ()
drawLockInfo al@(ActiveLock name i) Nothing _ = do
    let centre = hw<+>neg hv <+> 7*(i-1)<*>hu
    lift $ drawEmptyMiniLock centre
    drawNameWithCharAndCol name white (lockIndexChar i) (invisible white) centre
    lift $ registerSelectable centre 3 $ SelLockUnset al
drawLockInfo al@(ActiveLock name i) (Just lockinfo) mtvar = do
    let centre = hw<+>neg hv <+> 7*(i-1)<*>hu
    let accessedByPos = centre <+> 3<*>(hv <+> neg hw)
    let accessedPos = centre <+> 2<*>(hw <+> neg hv)
    let notesPos = centre <+> 3<*>(hw <+> neg hv)
    ourName <- (authUser <$>) <$> gets curAuth
    runMaybeT $ msum [
	do
	    tvar <- MaybeT $ return mtvar
	    lock <- MaybeT $ liftIO.atomically.readTVar $ tvar
	    lift.lift $ do
		drawMiniLock lock centre
		registerSelectable centre 3 $ SelLock al
	, lift $ do
		drawActiveLock al centre
		lift $ registerSelectable centre 3 $ SelLock al
	]

    lift.renderToMain $ renderStrColAt dimWhiteCol "Accessed by:" $ accessedByPos <+> hv
    if (public lockinfo) 
    then lift.renderToMain $ renderStrColAt (pixelToColor pubColour) "Everyone!" accessedByPos
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
	    then renderStrColAt (pixelToColor pubColour) "Accessed!"
	    else renderStrColAt (Color 0 0xff 0) "Solved!")
	accessedPos
    else if any (\(Undeclared _ ls _) -> ls == lockSpec lockinfo) undecls
    then lift.renderToMain $ renderStrColAt (Color 0xff 0xff 0) "Undeclared" accessedPos
    else do
	read <- take 3 <$> getNotesReadOn lockinfo
	when (not $ ourName == Just name) $ do
	    lift.renderToMain $ renderStrColAt (if length read == 3 then accessedCol else dimWhiteCol)
		"Read:" $ accessedPos <+> (-3)<*>hu
	    fillArea (accessedPos<+>neg hu) [ accessedPos <+> i<*>hu | i <- [-1..1] ]
		$ [ \pos -> (lift $ registerSelectable pos 0 (SelReadNote note)) >> drawName (noteAuthor note) pos
		    | note <- read ] ++ (repeat $ lift . renderToMain . drawAtRel (hollowGlyph $ dim $ colourWheel 2))

    lift.renderToMain $ renderStrColAt dimWhiteCol "Holds notes:" $ notesPos <+> hv
    if null $ notesSecured lockinfo
	then lift.renderToMain $ renderStrColAt messageCol "None" notesPos
	else fillArea notesPos
		[ notesPos <+> d | j <- [0..2], i <- [-2..3]
		    , i-j > -4, i-j < 3
		    , let d = j<*>hw <+> i<*>hu ]
		[ \pos -> (lift $ registerSelectable pos 0 (SelSecured note)) >> drawActiveLock (noteOn note) pos
		    | note <- notesSecured lockinfo ]

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
	    [ drawAtRel (buttonGlyph col) v >> renderStrColAt buttonTextCol bdg v |
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
    surf <- liftIO $ createRGBSurface [] w h 16 0 0 0 0
    modify $ \ds -> ds { gsSurface = Just surf, lastDrawArgs = Nothing }
    (_,size) <- getGeom
    fontpath <- liftIO $ getDataPath "VeraMono.ttf"
    font <- liftIO $ TTF.tryOpenFont fontpath (size)
    smallFont <- liftIO $ TTF.tryOpenFont fontpath (2*size`div`3)
    modify $ \ds -> ds { dispFont = font, dispFontSmall = smallFont }
    clearMiniLocks

drawMsgLine = void.runMaybeT $ do
    (col,str) <- msum
	[ ((,) dimWhiteCol) <$> MaybeT (gets hoverStr)
	, MaybeT $ gets message
	]
    lift $ do
	renderToMain $ blankRow messageLineStart
	renderToMain $ renderStrColAtLeft col str messageLineStart
setMsgLine col str = do
    modify $ \s -> s { message = Just (col,str) }
    when (not $ null str) $ modify $ \s -> s { hoverStr = Nothing }
    drawMsgLine >> refresh

drawTitle (Just title) = renderToMain $ renderStrColAt messageCol title titlePos
drawTitle Nothing = return ()
    
say = setMsgLine messageCol
sayError = setMsgLine errorCol

miniLockPos = (-9)<*>hw <+> hu
lockLinePos = 2<*>(hu <+> neg hw) <+> miniLockPos
serverPos = 12<*>hv <+> 6<*>neg hu
randomNamesPos = 9<*>hv <+> neg hu
codenamePos = (-4)<*>hw <+> 4<*>hv
undeclsPos = hv <+> neg hw <+> codenamePos
accessedOursPos = 2<*>hw <+> codenamePos

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawMainState = do
	lift $ clearButtons >> clearSelectables
	s <- get
	let mode = ms2im s
	lift $ waitFrame
	drawMainState' s
	lift $ do
	    drawButtons mode
	    drawUIOptionButtons mode
	    drawMsgLine
	    refresh
	where
	drawMainState' (PlayState st _ alerts wsel solved stack undostack title _) = lift $ do
	    let selTools = [ idx |
		    (idx, PlacedPiece pos p) <- enumVec $ placedPieces st
		    , or [wsel && isWrench p, not wsel && isHook p] ]
	    drawMainGameState selTools False alerts st
	    drawTitle title
	    registerUndoButtons (null stack) (null undostack)
	drawMainState' (ReplayState st todostack stack title) = lift $ do
	    drawMainGameState [] False [] st
	    drawTitle title
	    registerUndoButtons (null stack) (null todostack)
	    renderToMain $ drawCursorAt Nothing
	drawMainState' (EditState (st:sts) undostack _ _ selPiece selPos _) = lift $ do
	    drawMainGameState (maybeToList selPiece) True [] st
	    renderToMain $ drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	    registerUndoButtons (null sts) (null undostack)
	    drawPaintButtons
	drawMainState' (MetaState saddr undecls cOnly auth names _ rnamestvar _ lockTVs path lock) = do
	    let ourName = authUser <$> auth
	    let selName = listToMaybe names
	    let home = isJust ourName && ourName == selName
	    lift $ renderToMain $ (erase >> drawCursorAt Nothing)
	    maybe (lift $ drawEmptyMiniLock miniLockPos)
		(lift . (registerSelectable miniLockPos 3 SelOurLock >>) . flip drawMiniLock miniLockPos)
		lock
	    lift.renderToMain $ do
		renderStrColAtLeft messageCol (saddrStr saddr) $ serverPos <+> hu
		renderStrColAtLeft messageCol path $ lockLinePos <+> hu
	    maybe (return ())
		(\name -> do
		    lift $ registerSelectable codenamePos 0 SelSelectedCodeName 
		    drawName name codenamePos 
		    drawRelScore name (codenamePos<+>hu))
		selName
	    when (length names > 1) $ lift $ registerButtonGroup $ singleButton
		(codenamePos <+> 2<*>neg hu <+> hw) CmdBackCodename 0

	    runMaybeT $ MaybeT (return selName) >>= lift . getUInfoFetched 300 >>=
		\(FetchedRecord fresh err muirc) -> lift $ do
		    lift $ do
			when (not fresh) $ renderToMain $ drawAtRel unfreshGlyph $ codenamePos <+> 2<*>hu
			maybe (return ()) sayError err
			when (fresh && isNothing ourName) $
			    registerButtonGroup $ singleButton (codenamePos <+> 2<*>hu)
				(if isNothing muirc then CmdRegister else CmdAuth) 2
		    case muirc of
			Nothing -> return ()
			Just (RCUserInfo (_,uinfo)) -> do
			    sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) mlockinfo ((!i)<$>lockTVs) |
				(i,mlockinfo) <- assocs $ userLocks uinfo ]
			    let mmiscpos = (serverPos <+> 2<*>neg hv)
			    when (isJust $ msum $ elems $ userLocks uinfo) $ lift $ do
				registerButtonGroup
				    ([ Button mmiscpos (CmdSolve Nothing), Button (mmiscpos<+>hu) (CmdViewSolution Nothing)], (2,2))
				when (isJust lock && home) $ registerButtonGroup $
					singleButton (miniLockPos <+> 2<*>hv <+> neg hu) (CmdPlaceLock Nothing) 0

	    when home $ do
		when (not $ null undecls) $ do
		    lift.renderToMain $ renderStrColAtLeft messageCol "Undeclared:" (undeclsPos<+>2<*>hv)
		    lift $ registerButtonGroup $ singleButton (undeclsPos<+>2<*>hv<+>neg hu) (CmdDeclare Nothing) 2
		    fillArea (undeclsPos<+>hv)
			(map (undeclsPos<+>) $ hexDisc 1 ++ [hu<+>neg hw, neg hu<+>hv])
			[ \pos -> (lift $ registerSelectable pos 0 (SelUndeclared undecl)) >> drawActiveLock al pos
			| undecl@(Undeclared _ _ al) <- undecls ]
		rnames <- liftIO $ atomically $ readTVar rnamestvar
		when (not $ null rnames) $
		    fillArea randomNamesPos
			(map (randomNamesPos<+>) $ hexDisc 2)
			[ \pos -> (lift $ registerSelectable pos 0 (SelRandom name)) >> drawName name pos
			| name <- rnames ]

	    when (ourName /= selName) $ void $ runMaybeT $ do
		when (isJust ourName) $
		    lift.lift $ registerButtonGroup $ singleButton (codenamePos <+> neg hu <+> hw) CmdHome 4
		sel <- MaybeT $ return $ selName
		us <- MaybeT $ return $ ourName
		ourUInfo <- mgetUInfo us
		selUInfo <- mgetUInfo sel
		let accesses = map (\(name,ui) -> [ maybe (False,True)
			    (\lock -> (public lock, name `elem` accessedBy lock)) $ userLocks ui ! i
			    | i<-[0..2] ])
			[(sel,ourUInfo),(us,selUInfo)]
		lift $ do 
		    fillArea (codenamePos<+>3<*>hv<+>hw) (map ((codenamePos<+>3<*>hv)<+>) [zero,hw,neg hv])
			[ \pos -> (lift $ registerSelectable pos 0 SelOurAL) >>
			    drawNameWithCharAndCol us white (lockIndexChar i) col pos
			| i <- [0..2]
			, let (pub,access) = accesses !! 0 !! i
			, let col = if pub then obscure pubColour else if access then obscure $ scoreColour $ -3 else dim $ scoreColour 3 ]
		    fillArea (codenamePos<+>2<*>neg hw) (map ((codenamePos<+>3<*>neg hw)<+>) [zero,hw,neg hv])
			[ \pos -> (lift $ registerSelectable pos 0 (SelLock $ ActiveLock sel i)) >>
			    drawNameWithCharAndCol sel white (lockIndexChar i) col pos
			| i <- [0..2]
			, let (pub,access) = accesses !! 1 !! i
			, let col = if pub then obscure pubColour else if access then obscure $ scoreColour 3 else dim $ scoreColour $ -3 ]
	drawMainState' _ = return ()

    drawMessage prompt = say prompt
    drawError prompt = sayError prompt

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert _ = return ()

    showHelp _ = return ()

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
	    catchIOErrorMT m = MaybeT $ liftIO $ catchIO (m >> return (Just ())) (\_ -> return Nothing)

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
	    processEvent (MouseMotion _ _ _ _) = do
		(oldMPos,_) <- gets mousePos
		updateMousePos mode
		(mPos,_) <- gets mousePos
		if mode == IMEdit && mPos /= oldMPos then do
			centre <- gets dispCentre
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
		pos@(mPos,central) <- gets mousePos
		buttons <- (concat . map fst) `liftM` getButtons mode
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
		let isCentral = and $ map (\(a,b) -> abs ((fromIntegral a) - b) < 0.5) $
			[(x,sx),(y,sy),(z,sz)]
		let newPos = (mPos,isCentral)
		oldPos <- gets mousePos
		when (newPos /= oldPos) $ do
		    hstr <- (describeCommand<$>) <$> cmdAtMousePos newPos mode Nothing
		    modify $ \ds -> ds { mousePos = newPos, hoverStr = hstr }
