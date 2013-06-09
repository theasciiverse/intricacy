{-# LANGUAGE FlexibleInstances #-}
module SDLUI (UIM) where

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
import System.IO.Error

import UI
import Hex
import GameState
import Command
import KeyBindings

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
    filledPolygon surf corners col
    aaPolygon' surf corners $ bright col
    return ()
	where 
	    corners = concat [
		if dir `elem` adjs
		then [corner centre size $ (hextant dir) + n | n <- [0..1]]
		else [innerCorner centre size dir]
		| dir <- hexDirs ]

tileGlyph (SpringTile extn dir) col centre size surf =
    aaLines surf points $ bright col
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
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size) col
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size) $ bright col
    when (dir /= zero)
	$ void $ aaLine surf fromx fromy tox toy $ bright col
    return ()
	where
	    (fromx, fromy) = (edge centre size $ neg dir)
	    (tox, toy) = (edge centre size dir)

tileGlyph (ArmTile dir _) col centre@(SVec x y) size surf =
    void $ aaLine surf fromx fromy tox toy (bright col)
	where
	    (fromx, fromy) = edge centre size $ neg dir
	    (tox, toy) = innerCorner centre size dir

tileGlyph HookTile col centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size) col
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size) $ bright col
    return ()

tileGlyph (WrenchTile moving) col centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`2) col
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`2) $ bright col
    return ()

cursor :: Glyph
cursor centre size surf =
    aaPolygon' surf (map (corner centre size) [0..5]) $ bright white

bright :: Pixel -> Pixel
bright (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0xff

dim :: Pixel -> Pixel
dim (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0xa0

black = Pixel 0x01000000
white = Pixel 0xffffff00

colourWheel :: Int -> Pixel
colourWheel n = Pixel $ (((((r * 0x100) + g) * 0x100) + b) * 0x100) + a
    where [r,g,b] = map (\on -> if on then 0xff else 0) $ colourWheel' n
	  a = 0x00
	  colourWheel' 0 = [True, False, False]
	  colourWheel' 1 = [True, True, False]
	  colourWheel' n = let [a,b,c] = colourWheel' $ n-2 in [b,c,a]

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring highlight (owner,t) =
    let col = (if owner `elem` highlight then bright else dim) $
	    case Map.lookup owner colouring of
		    Nothing -> white
		    Just n -> colourWheel n
    in tileGlyph t col

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

sVec2HexVec :: Int -> SVec -> HexVec
sVec2HexVec size (SVec x y) =
    let size' = fromIntegral size
	x' = fromIntegral x
	y' = fromIntegral y
	hy' = -y' / size' / (sqrt 3)
	hx' = ((x' / size') - hy') / 2
	hx = round hx'
	hy = round hy'
	hz = -hx-hy
    in HexVec hx hy hz

data UIState = UIState { scrHeight::Int, scrWidth::Int, dispCentre::HexPos, dispLastCol::PieceColouring }
type UIM = StateT UIState IO
nullUIState = UIState 0 0 (PHS zero) Map.empty

erase :: UIM ()
erase = do
    surface <- liftIO getVideoSurface
    liftIO $ fillRect surface Nothing black
    return ()

refresh :: UIM ()
refresh = do 
    surface <- liftIO getVideoSurface
    liftIO $ SDL.flip surface

drawAt :: Glyph -> HexPos -> UIM ()
drawAt gl pos = do
	(scrCentre, size) <- getGeom
	centre <- gets dispCentre
	surface <- liftIO getVideoSurface
	let cpos = scrCentre <+> (hexVec2SVec size (pos <-> centre))
	liftIO $ gl cpos size surface

drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt (Just pos) = drawAt cursor pos
drawCursorAt _ = return ()

getGeom :: UIM (SVec, Int)
getGeom = do 
    h <- gets scrHeight 
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    let size = minimum [ w`div`80, floor $ (fromIntegral w) / ((sqrt 3)*25) ]
    return (scrCentre, size)


drawState :: [PieceIdx] -> Bool -> GameState -> UIM ()
drawState highlight colourFixed st =
    let board = stateBoard st
    in do
	lastCol <- gets dispLastCol
	let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
	modify $ \ds -> ds { dispLastCol = colouring }
	erase
	sequence_ [ drawAt glyph pos | 
	    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring highlight) board
	    ]

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawPlayState (PlayState (st:_) wsel _) = do
	let selTools = [ idx |
		(idx, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]
	drawState selTools False st
	refresh

    drawEditState (EditState (st:_) selPiece selPos _) = do
	drawState (maybeToList selPiece) True st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	refresh

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert (AlertCollision pos) = return ()

    initUI = let {w=800; h=640} in do
	ok <- liftIO $ catchIOError (SDL.init [InitVideo] >> return True) (\_ -> return False)
	if not ok
	    then return False
	    else do
		liftIO $ setVideoMode w h 32 []
		modify $ \ds -> ds { scrWidth = w }
		modify $ \ds -> ds { scrHeight = h }
		liftIO $ warpMouse (fromIntegral $ w`div`2) (fromIntegral $ h`div`2)
		erase
		liftIO $ enableUnicode True
		return True

    endUI = liftIO $ quit

    getInput mode = do
	event <- liftIO $ waitEvent
	case event of
	    KeyDown (Keysym _ _ ch) ->
		return $ maybeToList $ Map.lookup ch $ bindings mode
	    MouseMotion x y _ _ -> do
		let mpos = SVec (fromIntegral x) (fromIntegral y)
		centre <- gets dispCentre
		(scrCentre, size) <- getGeom
		return [ CmdMoveTo $ (sVec2HexVec size $ mpos <+> neg scrCentre) <+> centre ]
	    _ -> return []
