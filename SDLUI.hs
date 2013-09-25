{-# LANGUAGE FlexibleInstances #-}
module SDLUI (UIM) where

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as TTF
import Graphics.UI.SDL.Primitives
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State
import System.IO.Error
import Data.Word
import Data.List (maximumBy, findIndex, elemIndex)
import Text.Show.Pretty (ppShow)
--import Debug.Trace (traceShow)

import UI
import Hex
import GameState
import Physics
import Command
import KeyBindings
import Mundanities

maxlocksize = 12 -- TODO: use actual lock size?
periphery 0 = ((3*maxlocksize)`div`2)<*>hu <+> ((3*maxlocksize)`div`4)<*>hv
periphery n = rotate n $ periphery 0

-- aaPolygon seems to be a bit buggy in sdl-gfx-0.6.0
aaPolygon' surf verts col =
    aaLines surf (verts ++ take 1 verts) col
-- aaCircle too
aaCircle' surf x y rad col =
    if (rad == 1) then pixel surf x y col
    else aaCircle surf x y rad col

aaLines surf verts col =
    sequence_ [ aaLine surf x y x' y' col |
	((x,y),(x',y')) <- zip (take (length verts - 1) verts) (drop 1 verts) ]

rimmedPolygon surf verts fillCol rimCol = do
    filledPolygon surf verts fillCol
    aaPolygon' surf verts rimCol
    return ()

rimmedCircle surf centre@(SVec x y) rad fillCol rimCol = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral rad) fillCol
    aaCircle' surf (fromIntegral x) (fromIntegral y) (fromIntegral rad) rimCol
    return ()

type Glyph = SVec -> Int -> Surface -> IO ()

corner :: Integral i => SVec -> Int -> Int -> (i,i)
corner (SVec x y) size hextant = (fromIntegral $ x+dx, fromIntegral $ y+dy)
    where
	[dx,dy] = map round $ f hextant
	f 0 = [sizeFloat, -(1 / (sqrt 3)) * sizeFloat]
	f 1 = [0, -(2 / (sqrt 3)) * sizeFloat]
	f 2 = [-sizeFloat, -(1 / (sqrt 3)) * sizeFloat]
	f n | n < 6 = let [x,y] = f (5-n) in [x,-y]
	    | n < 0 = f (6-n)
	    | otherwise = f (n`mod`6)
	sizeFloat = fromIntegral size

innerCorner :: Integral i => SVec -> Int -> HexDir -> (i,i)
innerCorner (SVec x y) size dir = (fromIntegral $ x+dx, fromIntegral $ y+dy)
    where
	[dx,dy] = map round $ f dir
	f dir
	    | dir == hu = [2*sizeFloat/3, 0]
	    | dir == hv = [-sizeFloat/3, - (1 / (sqrt 3)) * sizeFloat]
	    | dir == hw = [-sizeFloat/3, (1 / (sqrt 3)) * sizeFloat]
	    | not (isHexDir dir) = error "innerCorner: not a hexdir"
	    | otherwise = map (\z -> -z) $ f $ neg dir
	sizeFloat = fromIntegral size


edge :: Integral i => SVec -> Int -> HexDir -> (i,i)
edge (SVec x y) size dir = (fromIntegral $ x+dx, fromIntegral $ y+dy)
    where
	[dx,dy] = map round $ f dir
	y' = ((sqrt 3) / 2) * sizeFloat
	x' = sizeFloat / 2
	f dir
	    | dir == hu = [sizeFloat, 0]
	    | dir == hv = [-x', -y']
	    | dir == hw = [-x', y']
	    | not (isHexDir dir) = error "edge: not a hexdir"
	    | otherwise = map (\z -> -z) $ f $ neg dir
	sizeFloat = fromIntegral $ size - 1

tileGlyph :: Tile -> Pixel -> Glyph

tileGlyph (BlockTile adjs) col centre size surf =
    rimmedPolygon surf corners col $ bright col
	where 
	    corners = concat [
		if dir `elem` adjs
		then [corner centre size $ (hextant dir) + n | n <- [-1,0]]
		else [innerCorner centre size dir]
		| dir <- hexDirs ]

tileGlyph (SpringTile extn dir) col centre size surf =
    aaLines surf points $ bright col
	where
	    n = case extn of
		Stretched -> 2
		Relaxed -> 4
		Compressed -> 6
	    dir' = if dir == zero then hu else dir
	    (sx,sy) = corner centre size (hextant dir' - 1)
	    (offx,offy) = corner centre size (hextant dir')
	    (ex,ey) = corner centre size (hextant dir' - 3)
	    points = [ (x+dx,y+dy)
		| i <- [0..n]
		, let (x,y) = if even i then (sx,sy) else (offx,offy)
		, let (dx,dy) = ((i*(ex-sx))`div`n, (i*(ey-sy))`div`n) ]

tileGlyph (PivotTile dir) col centre size surf = do
    rimmedCircle surf centre rad col $ bright col
    when (dir /= zero)
	$ void $ aaLine surf `uncurry` from `uncurry` to $ bright col
    return ()
	where
	    rad = (7*size)`div`8
	    from = edge centre rad $ neg dir
	    to = edge centre rad dir

tileGlyph (ArmTile dir _) col centre size surf =
    void $ aaLine surf `uncurry` from `uncurry` to $ bright col
	where
	    dir' = if dir == zero then hu else dir
	    from = edge centre size $ neg dir'
	    to = innerCorner centre size dir'

tileGlyph HookTile col centre size surf =
    rimmedCircle surf centre rad col $ bright col
	where
	    rad = (7*size)`div`8

tileGlyph (WrenchTile mom) col centre size surf = do
    rimmedCircle surf centre (size`div`3) col $ bright col
    when (mom /= zero) $
	let 
	    from@(fx,fy) = innerCorner centre size $ neg mom
	    to@(tx,ty) = edge centre size $ neg mom
	    shifts = map (map (`div` 2)) $
		[ [ x1-x0, y1-y0 ]
		| rot <- [-1,0,1]
		, let (x0,y0) = innerCorner centre size $ neg mom
		, let (x1,y1) = innerCorner centre size $ rotate rot $ neg mom
		]
	in sequence_
	    [ aaLine surf (fx+dx) (fy+dy) (tx+dx) (ty+dy) $ col
	    | [dx,dy] <- shifts ]

tileGlyph BallTile col centre size surf =
    rimmedCircle surf centre size (faint col) (obscure col)

blockedArm :: HexDir -> TorqueDir -> Pixel -> Glyph
blockedArm armdir tdir col centre size surf =
    void $ aaLine surf `uncurry` from `uncurry` to $ col
	where
	    from = innerCorner centre size $ rotate (2*tdir) armdir
	    to = edge centre size $ rotate tdir armdir

blockedBlock :: Tile -> HexDir -> Pixel -> Glyph
blockedBlock tile dir col centre size =
    tileGlyph tile col (shift <+> centre) size
	where
	    shift = SVec (x'-x) (y'-y)
	    (x,y) = innerCorner centre size dir
	    (x',y') = edge centre size dir

blockedPush :: HexDir -> Pixel -> Glyph
blockedPush dir col centre size surf = do
{-
    void $ rimmedPolygon surf verts (obscure col) (dim col)
	where verts =
		[ innerCorner centre size $ rotate 1 dir
		, innerCorner centre size $ rotate (-1) dir
		, innerCorner centre size dir ]
-}
    void $ aaLine surf `uncurry` base `uncurry` head $ col
    --void $ aaLine surf `uncurry` from' `uncurry` to' $ col
    void $ aaLine surf `uncurry` head `uncurry` (arms!!0) $ col
    void $ aaLine surf `uncurry` head `uncurry` (arms!!1) $ col
    where
	--base@(bx,by) = innerCorner centre size dir
	SVec bx' by' = centre
	base@(bx,by) = (fromIntegral bx',fromIntegral by')
	head@(hx,hy) = edge centre size dir
	arms = [(bx + (hx-bx)`div`2 + dir*(hy-by)`div`4,
	    by + (hy-by)`div`2 - dir*(hx-bx)`div`4) | dir <- [-1,1]]

	--from' = corner centre size $ hextant dir
	--to' = corner centre size $ (hextant dir) - 1
	
collisionMarker :: Glyph
collisionMarker centre size surf =
    rimmedCircle surf centre (size`div`3) (bright orange) $ bright orange

hollowGlyph :: Pixel -> Glyph
hollowGlyph col centre size surf =
    aaPolygon' surf corners $ bright col
    where corners = [ innerCorner centre size dir | dir <- hexDirs ]

buttonGlyph :: Pixel -> Glyph
buttonGlyph = tileGlyph (BlockTile [])

useFiveColourButton :: Bool -> Glyph
useFiveColourButton using centre@(SVec x y) size surf = do
    mapM_ (\h -> tileGlyph (BlockTile [])
	    (dim $ colourWheel (if using then h`div`2 else 0))
	    (SVec `uncurry` corner centre (size`div`2) h) (size`div`2) surf)
	[0,2,4]

showBlocksButton :: Bool -> Glyph
showBlocksButton showing centre size surf = do
    tileGlyph (BlockTile []) (dim $ colourWheel 0) centre size surf
    when showing $ blockedPush hu (bright orange) centre size surf

whsButtonsButton :: WrHoSel -> Glyph
whsButtonsButton whs centre size surf = do
    when (whs /= WHSHook) $
	tileGlyph (WrenchTile zero) col (miniCentre 0) miniSize surf
    when (whs /= WHSWrench) $ do
	tileGlyph HookTile col (miniCentre 4) miniSize surf
	tileGlyph (ArmTile hv False) col (miniCentre 2) miniSize surf
    where
	miniSize = size `div` 2
	miniCentre h = SVec `uncurry` corner centre miniSize h
	col = dim white

cursor :: Glyph
cursor centre size surf =
    aaPolygon' surf (map (corner centre size) [0..5]) $ bright white

bright :: Pixel -> Pixel
bright (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0xff

dim :: Pixel -> Pixel
dim (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0xa0

obscure :: Pixel -> Pixel
obscure (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0x80

faint :: Pixel -> Pixel
faint (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0x40

black = Pixel 0x01000000
white = Pixel 0xffffff00
orange = Pixel 0xff7f0000

colourWheel :: Int -> Pixel
colourWheel n = Pixel $ (((((r * 0x100) + g) * 0x100) + b) * 0x100) + a
    where [r,g,b] = map (\on -> if on then 0xff else 0) $ colourWheel' n
	  a = 0x00
	  colourWheel' 0 = [True, False, False]
	  colourWheel' 1 = [True, True, False]
	  colourWheel' n = let [a,b,c] = colourWheel' $ n-2 in [c,a,b]

colourOf colouring idx = 
    case Map.lookup idx colouring of
	    Nothing -> white
	    Just n -> colourWheel n

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring highlight (owner,t) =
    let col = colourOf colouring owner
    in tileGlyph t $ (if owner `elem` highlight then bright else dim) col

data SVec = SVec { cx, cy :: Int }
    deriving (Eq, Ord, Show)
instance Monoid SVec where
    mempty = SVec 0 0
    mappend (SVec x y) (SVec x' y') = SVec (x+x') (y+y') 
instance Grp SVec where
    neg (SVec x y) = SVec (-x) (-y)
type CCoord = PHS SVec

hexVec2SVec :: Int -> HexVec -> SVec
hexVec2SVec size (HexVec x y z) =
    SVec ((x-z) * size) (floor $ -(fromIntegral $ y * size) * (sqrt 3))

sVec2dHV :: Int -> SVec -> (Double,Double,Double)
sVec2dHV size sv@(SVec x y) =
    let size' = fromIntegral size
	x' = fromIntegral x
	y' = fromIntegral y
	hy' = -y' / size' / (sqrt 3)
	hx' = ((x' / size') - hy') / 2
	hz' = -((x' / size') + hy') / 2
    in (hx',hy',hz')

sVec2HexVec :: Int -> SVec -> HexVec
sVec2HexVec size sv =
    let (hx',hy',hz') = sVec2dHV size sv
	unrounded = Map.fromList [(1,hx'),(2,hy'),(3,hz')]
	rounded = Map.map round unrounded
	maxdiff = fst $ maximumBy (\x y -> compare (snd x) (snd y)) $
	    [ (i, abs $ x'-x) | i <- [1..3],
		let x' = unrounded Map.! i, let x = fromIntegral $ rounded Map.! i]
	[hx,hy,hz] = map snd $ Map.toList $
	    Map.adjust (\x -> x - (sum $ Map.elems rounded)) maxdiff rounded
    in HexVec hx hy hz

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

erase :: UIM ()
erase = do
    surface <- liftIO getVideoSurface
    liftIO $ fillRect surface Nothing black
    return ()

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

drawAt :: Glyph -> HexPos -> UIM ()
drawAt gl pos = do
	centre <- gets dispCentre
	drawAtRel gl (pos <-> centre)
drawAtRel gl v = do
	(scrCentre, size) <- getGeom
	surface <- liftIO getVideoSurface
	let cpos = scrCentre <+> (hexVec2SVec size v)
	liftIO $ gl cpos size surface

renderStrAt :: String -> HexDir -> UIM ()
renderStrAt str v = do
    mfont <- gets dispFont
    case mfont of
	Nothing -> return ()
	Just font -> do
	    mfsurf <- liftIO $ TTF.tryRenderTextBlended font str (Color 0xff 0xff 0xff)
	    case mfsurf of
		Nothing -> return ()
		Just fsurf -> do
		    (scrCentre, size) <- getGeom
		    centre <- gets dispCentre
		    let SVec x y = scrCentre <+> (hexVec2SVec size v)
		    let x' = x - (surfaceGetWidth fsurf)`div`2
		    let y' = y - (surfaceGetHeight fsurf)`div`2
		    surface <- liftIO getVideoSurface
		    void $ liftIO $ blitSurface fsurf Nothing surface (Just $ Rect x' y' 0 0)

drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt (Just pos) = drawAt cursor pos
drawCursorAt _ = return ()

drawBlocked :: GameState -> PieceColouring -> Bool -> Force -> UIM ()
drawBlocked st colouring blocking (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
	    PlacedPiece pos (Pivot arms) -> (pos,arms)
	    PlacedPiece pos (Hook arm _) -> (pos,[arm])
	col = if blocking then bright $ colourWheel 5 else dim $ colourOf colouring idx
    sequence_ [ drawAt (blockedArm arm dir col) (arm <+> pos) |
	arm <- arms ]
drawBlocked st colouring blocking (Push idx dir) = do
    let footprint = plPieceFootprint $ getpp st idx
	fullfootprint = fullFootprint st idx
	col = if blocking then bright $ colourWheel 5 else bright orange
    sequence_ [ drawAt (blockedPush dir $ col) pos
	| pos <- footprint
	, not $ (dir<+>pos) `elem` fullfootprint ]
    -- drawAt (blockedPush dir $ bright orange) $ placedPos $ getpp st idx

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
	IMConfirm -> [
	    ([ quitButton
	    , Button (br<+>hw) CmdYes
	    , Button (br<+>(neg hv)) CmdNo
	    ],(0,2)) ]
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
    drawAtRel (uiOptGlyph b value) $ uiOptPos b
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
	Just t -> drawAtRel (tileGlyph t (dim $ colourWheel 5)) (periphery 0)

drawPaintButtons :: UIM ()
drawPaintButtons = do
    pti <- getEffPaintTileIndex
    sequence_ [ 
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

drawState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawState highlight colourFixed alerts st = do
    uiopts <- gets uiOptions
    drawState' $ DrawArgs highlight colourFixed alerts st uiopts

drawState' :: DrawArgs -> UIM ()
drawState' args@(DrawArgs highlight colourFixed alerts st uiopts) = do
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
	    gsSurf <- liftM fromJust $ gets gsSurface
	    liftIO $ blitSurface vidSurf Nothing gsSurf Nothing

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
    sequence_ $ concat [
	    [ drawAtRel (buttonGlyph col) v >> renderStrAt bdg v |
		(i,(v,bdg)) <- enumerate $ map (\b->(buttonPos b, bindingStr $ buttonCmd b)) $ buttonGroup
		, let col = dim $ colourWheel (base+inc*i) ]
	| (buttonGroup,(base,inc)) <- buttons
	]
    where enumerate = zip [0..]

initVideo :: Int -> Int -> UIM ()
initVideo w h = do
    liftIO $ setVideoMode w h 32 [Resizable]
    modify $ \ds -> ds { scrWidth = w }
    modify $ \ds -> ds { scrHeight = h }
    surf <- liftIO $ createRGBSurface [ SWSurface ] w h 32 0xff000000 0xff0000 0xff00 0
    modify $ \ds -> ds { gsSurface = Just surf, lastDrawArgs = Nothing }
    (_,size) <- getGeom
    font <- liftIO $ TTF.tryOpenFont "VeraMono.ttf" (size)
    modify $ \ds -> ds { dispFont = font }

say str = renderStrAt str $ periphery 0

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawPlayState (PlayState st alerts wsel solved _ _ _) = do
	waitFrame
	let selTools = [ idx |
		(idx, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]
	drawState selTools False alerts st
	drawButtons IMPlay
	drawUIOptionButtons IMPlay
	when solved $ say "Unlocked!"
	refresh

    drawEditState (EditState (st:_) _ selPiece selPos _ _) = do
	waitFrame
	drawState (maybeToList selPiece) True [] st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	-- drawPaintSel
	drawButtons IMEdit
	drawPaintButtons
	drawUIOptionButtons IMEdit
	refresh

    askYesNo prompt = do
	waitFrame
	drawButtons IMConfirm
	refresh

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert _ = return ()

    initUI = let {w=800; h=640} in do
	ok <- liftIO $ catchIOError (SDL.init [InitVideo] >> return True) (\_ -> return False)
	if not ok
	then return False
	else do
	    liftIO TTF.init
	    initVideo w h
	    liftIO $ warpMouse (fromIntegral $ w`div`2) (fromIntegral $ h`div`2)
	    erase
	    liftIO $ enableUnicode True
	    liftIO $ enableKeyRepeat 500 30
	    readUIConfigFile
	    readBindings
	    return True

    endUI = do
	writeUIConfigFile
	writeBindings
	liftIO $ quit

    getInput mode = do
	events <- liftIO $ getEvents
	concat `liftM` (sequence $ map processEvent events)
	where
	    processEvent (KeyDown (Keysym _ _ ch)) = do
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
	    processEvent (VideoResize w h) = do
		initVideo w h
		return [ CmdRedraw ]
		
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
