module SDLDisplay ( drawState, drawPlayState, drawEditState, drawAlerts
    , DisplayState(DisplayState), DisplayM, nullDisplayState, initDisplay, endDisplay, getKey ) where

import Graphics.UI.SDL
import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Primitives
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State

import Hex
import GameState

-- aaPolygon seems to be a bit buggy in sdl-gfx-0.6.0
aaPolygon' surf verts col =
    aaLines surf (verts ++ take 1 verts) col
aaLines surf verts col =
    sequence_ [ aaLine surf x y x' y' col |
	((x,y),(x',y')) <- zip (take (length verts - 1) verts) (drop 1 verts) ]
	

type Glyph = SVec -> Int -> Surface -> IO ()

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

tileGlyph (BlockTile adjs) col centre size surf = do
    --filledPolygon surf (map (corner centre size) [0..5]) colDim
    --aaPolygon surf (map (corner centre size) [0..5]) colDim
    filledPolygon surf corners colDim
    aaPolygon' surf corners col
    return ()
	where 
	    corners = concat [
		if dir `elem` adjs
		then [corner centre size $ (hextant dir) + n | n <- [0..1]]
		else [innerCorner centre size dir]
		| dir <- hexDirs ]
	    colDim = let Pixel v = col in Pixel $ v `div` 0x100 * 0x100 + 0x80

tileGlyph (SpringTile extn dir) col centre size surf =
    aaLines surf points col
	where
	    n = case extn of
		Stretched -> 2
		Relaxed -> 4
		Compressed -> 6
	    (sx,sy) = corner centre size (hextant dir)
	    (offx,offy) = corner centre size (hextant dir + 1)
	    (ex,ey) = corner centre size (hextant dir - 2)
	    points = [ (x+dx,y+dy)
		| i <- [0..n]
		, let (x,y) = if even i then (sx,sy) else (offx,offy)
		, let (dx,dy) = ((i*(ex-sx))`div`n, (i*(ey-sy))`div`n) ]

tileGlyph (PivotTile dir) col centre@(SVec x y) size surf = do
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size - 1) col
    when (dir /= zero)
	$ aaLine surf (fromIntegral x) (fromIntegral y) tox toy col >> return ()
    return ()
	where (tox, toy) = (edge centre size dir)

tileGlyph (ArmTile dir _) col centre@(SVec x y) size surf = do
    aaLine surf fromx fromy tox toy col >> return ()
    return ()
	where
	    (fromx, fromy) = edge centre size $ neg dir
	    (tox, toy) = innerCorner centre size dir

tileGlyph HookTile col centre@(SVec x y) size surf = do
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size - 1) col
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size - 1) col
    return ()

tileGlyph (WrenchTile moving) col centre@(SVec x y) size surf = do
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`2) col
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`2) col
    return ()

black = Pixel 0xff000000
white = Pixel 0xffffffff

colourWheel :: Int -> Pixel
colourWheel n = Pixel $ (((((r * 0x100) + g) * 0x100) + b) * 0x100) + a
    where [r,g,b] = map (\on -> if on then 0xff else 0) $ colourWheel' n
	  a = 0xff
	  colourWheel' 0 = [True, False, False]
	  colourWheel' 1 = [True, True, False]
	  colourWheel' n = let [a,b,c] = colourWheel' $ n-2 in [b,c,a]

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring reversed (owner,t) =
    let col = case Map.lookup owner colouring of
		    Nothing -> white
		    Just n -> colourWheel n
	reverse = owner `elem` reversed
    in tileGlyph t col

data SVec = SVec { cx, cy :: Int }
    deriving (Eq, Ord, Show)
instance Monoid SVec where
    mempty = SVec 0 0
    mappend (SVec x y) (SVec x' y') = SVec (x+x') (y+y') 
instance Grp SVec where
    neg (SVec x y) = SVec (-x) (-y)
type CCoord = PHS SVec

hexVec2SVec :: HexVec -> Int -> SVec
hexVec2SVec (HexVec x y z) size = SVec ((x-z) * size) (floor $ -(fromIntegral $ y * size) * (sqrt 3))

data DisplayState = DisplayState { scrHeight::Int, scrWidth::Int, dispCentre::HexPos, dispLastCol::PieceColouring }
type DisplayM = StateT DisplayState IO
nullDisplayState = DisplayState 0 0 (PHS zero) Map.empty


erase :: DisplayM ()
erase = do
    surface <- liftIO getVideoSurface
    liftIO $ fillRect surface Nothing black
    return ()

refresh :: DisplayM ()
refresh = do 
    surface <- liftIO getVideoSurface
    liftIO $ SDL.flip surface

drawAt :: Glyph -> HexPos -> DisplayM ()
drawAt gl pos = do
	(scrCentre, size) <- getGeom
	centre <- gets dispCentre
	surface <- liftIO getVideoSurface
	let cpos = scrCentre <+> (hexVec2SVec (pos <-> centre) size)
	liftIO $ gl cpos size surface

drawCursorAt :: Maybe HexPos -> DisplayM ()
drawCursorAt _ = return ()

getGeom :: DisplayM (SVec, Int)
getGeom = do 
    h <- gets scrHeight 
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    let size = minimum [ w`div`80, floor $ (fromIntegral w) / ((sqrt 3)*25) ]
    return (scrCentre, size)


drawState :: [PieceIdx] -> Bool -> GameState -> DisplayM ()
drawState reversed colourFixed st =
    let board = stateBoard st
    in do
	lastCol <- gets dispLastCol
	let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
	modify $ \ds -> ds { dispLastCol = colouring }
	erase
	sequence_ [ drawAt glyph pos | 
	    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring reversed) board
	    ]
	refresh

drawPlayState :: PlayState -> DisplayM ()
drawPlayState (PlayState (st:_) wsel) =
    do
	drawState [] False st
	drawCursorAt $ listToMaybe [ pos |
	    (_, PlacedPiece pos p) <- enumVec $ placedPieces st
	    , or [wsel && isWrench p, not wsel && isHook p] ]

drawEditState :: EditState -> DisplayM ()
drawEditState (EditState (st:_) selPiece selPos) =
    do
	drawState (maybeToList selPiece) True st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing

drawAlerts :: [Alert] -> DisplayM ()
drawAlerts alerts =
    do mapM_ drawAlert alerts
       when (not $ null alerts) 
	$ do refresh
	     liftIO $ threadDelay $ 5*10^4
    where
	drawAlert (AlertCollision pos) = return ()

initDisplay :: DisplayM ()
initDisplay = let {w=800; h=640} in do
    liftIO $ SDL.init [InitVideo]
    liftIO $ setVideoMode w h 32 []
    modify $ \ds -> ds { scrWidth = w }
    modify $ \ds -> ds { scrHeight = h }
    erase

endDisplay :: DisplayM ()
endDisplay = liftIO $ quit

getKey :: DisplayM Char
getKey = return ' '
