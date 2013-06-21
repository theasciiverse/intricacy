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
import Data.Word

import UI
import Hex
import GameState
import Physics
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
	    dir' = if dir == zero then hu else dir
	    (fromx, fromy) = edge centre size $ neg dir'
	    (tox, toy) = innerCorner centre size dir'

tileGlyph HookTile col centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size) col
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size) $ bright col
    return ()

tileGlyph (WrenchTile mom) col centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`2) col
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`2) $ bright col
    when (mom /= zero) $
	let 
	    (fromx, fromy) = innerCorner centre size $ neg mom
	    (tox, toy) = edge centre size $ neg mom
	in void $ aaLine surf fromx fromy tox toy col
    return ()

blockedArm :: HexDir -> TorqueDir -> Pixel -> Glyph
blockedArm armdir tdir col centre@(SVec x y) size surf =
    void $ aaLine surf fromx fromy tox toy col
	where
	    (fromx, fromy) = innerCorner centre size $ rotate (2*tdir) armdir
	    (tox, toy) = edge centre size $ rotate tdir armdir

blockedBlock :: Tile -> HexDir -> Pixel -> Glyph
blockedBlock tile dir col centre size = tileGlyph tile col (shift <+> centre) size
    where
	shift = SVec (x'-x) (y'-y)
	(x,y) = innerCorner centre size dir
	(x',y') = edge centre size dir
	
collisionMarker :: Glyph
collisionMarker centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`3) $ bright orange
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`3) $ bright orange
    return ()

cursor :: Glyph
cursor centre size surf =
    aaPolygon' surf (map (corner centre size) [0..5]) $ bright white

bright :: Pixel -> Pixel
bright (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0xff

dim :: Pixel -> Pixel
dim (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0xa0

obscure :: Pixel -> Pixel
obscure (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + 0x80

black = Pixel 0x01000000
white = Pixel 0xffffff00
orange = Pixel 0xff7f0000

colourWheel :: Int -> Pixel
colourWheel n = Pixel $ (((((r * 0x100) + g) * 0x100) + b) * 0x100) + a
    where [r,g,b] = map (\on -> if on then 0xff else 0) $ colourWheel' n
	  a = 0x00
	  colourWheel' 0 = [True, False, False]
	  colourWheel' 1 = [True, True, False]
	  colourWheel' n = let [a,b,c] = colourWheel' $ n-2 in [b,c,a]

colourOf colouring idx = 
    case Map.lookup idx colouring of
	    Nothing -> white
	    Just n -> colourWheel n

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring highlight (owner,t) =
    let col = (if owner `elem` highlight then bright else dim) $
	    colourOf colouring owner
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
	hz' = -((x' / size') + hy') / 2
	unrounded = Map.fromList [(1,hx'),(2,hy'),(3,hz')]
	rounded = Map.map round unrounded
	maxdiff = fst $ Map.findMax $ Map.fromList [ (i, abs $ x'-x) | i <- [1..3],
	    let x' = unrounded Map.! i, let x = fromIntegral $ rounded Map.! i]
	[hx,hy,hz] = map snd $ Map.toList $
	    Map.adjust (\x -> x - (sum $ Map.elems rounded)) maxdiff rounded
    in HexVec hx hy hz


data UIState = UIState { scrHeight::Int, scrWidth::Int
    , lastFrameTicks::Word32, paintTile::Maybe Tile
    , leftButtonDown::Bool, rightButtonDown::Bool
    , mousePos::HexVec
    , dispCentre::HexPos, dispLastCol::PieceColouring }
type UIM = StateT UIState IO
nullUIState = UIState 0 0 0 (Just $ BlockTile []) False False zero (PHS zero) Map.empty

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
	(scrCentre, size) <- getGeom
	centre <- gets dispCentre
	surface <- liftIO getVideoSurface
	let cpos = scrCentre <+> (hexVec2SVec size (pos <-> centre))
	liftIO $ gl cpos size surface

drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt (Just pos) = drawAt cursor pos
drawCursorAt _ = return ()

drawBlocked :: GameState -> PieceColouring -> Force -> UIM ()
drawBlocked st colouring (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
	    PlacedPiece pos (Pivot arms) -> (pos,arms)
	    PlacedPiece pos (Hook arm _) -> (pos,[arm])
	col = obscure $ colourOf colouring idx
    sequence_ [ drawAt (blockedArm arm dir col) (arm <+> pos) |
	arm <- arms ]
drawBlocked st colouring (Push idx dir) = do
    return ()
{-
    let pmap = plPieceMap $ getpp st idx
	col = obscure $ colourOf colouring idx
    sequence_ [ drawAt (blockedBlock tile dir col) pos |
	(pos,tile) <- Map.toList pmap ]
-}

drawPaintSel :: UIM ()
drawPaintSel = do
    pTile <- gets paintTile
    case pTile of
	Nothing -> return ()
	Just t -> do
	    (scrCentre, size) <- getGeom
	    w <- gets scrWidth
	    h <- gets scrHeight
	    surface <- liftIO getVideoSurface
	    liftIO $ tileGlyph t (dim white)
		(SVec (3*w `div` 4) (h `div` 5)) size surface

getGeom :: UIM (SVec, Int)
getGeom = do 
    h <- gets scrHeight 
    w <- gets scrWidth
    let scrCentre = SVec (w`div`2) (h`div`2)
    let size = minimum [ w`div`80, floor $ (fromIntegral h) / ((sqrt 3)*25) ]
    return (scrCentre, size)


drawState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawState highlight colourFixed alerts st =
    let board = stateBoard st
    in do
	lastCol <- gets dispLastCol
	let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
	modify $ \ds -> ds { dispLastCol = colouring }
	erase
	sequence_ [ drawAt glyph pos | 
	    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring highlight) board
	    ]
	sequence_ $ [ drawBlocked st colouring force | 
	    AlertBlockedForce force <- alerts ]
	    -- ++ [ drawAt collisionMarker pos | AlertCollision pos <- alerts ]

initVideo :: Int -> Int -> UIM ()
initVideo w h = do
    liftIO $ setVideoMode w h 32 [Resizable]
    modify $ \ds -> ds { scrWidth = w }
    modify $ \ds -> ds { scrHeight = h }

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawPlayState (PlayState (st:_) alerts wsel _) = do
	waitFrame
	let selTools = [ idx |
		(idx, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]
	drawState selTools False alerts st
	refresh

    drawEditState (EditState (st:_) selPiece selPos _ _) = do
	waitFrame
	drawState (maybeToList selPiece) True [] st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	drawPaintSel
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
		initVideo w h
		liftIO $ warpMouse (fromIntegral $ w`div`2) (fromIntegral $ h`div`2)
		erase
		liftIO $ enableUnicode True
		return True

    endUI = liftIO $ quit

    getInput mode = do
	events <- liftIO $ getEvents
	concat `liftM` (sequence $ map processEvent events)
	where
	    processEvent (KeyDown (Keysym _ _ ch)) =
		return $ maybeToList $ Map.lookup ch $ bindings mode
	    processEvent (MouseMotion x y _ _) = do
		(scrCentre, size) <- getGeom
		let mPos = sVec2HexVec size $ (SVec (fromIntegral x) (fromIntegral y)) <+> neg scrCentre
		oldMPos <- gets mousePos
		if mPos /= oldMPos
		    then do
			modify $ \s -> s {mousePos = mPos}
			centre <- gets dispCentre
			pTile <- gets paintTile
			lb <- gets leftButtonDown
			return $ [ CmdMoveTo $ mPos <+> centre ] ++
			    if lb then [ paintCmd pTile ] else []
		    else return []
	    processEvent (MouseButtonDown _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = True }
		pTile <- gets paintTile
		return $ [ paintCmd pTile ]
	    processEvent (MouseButtonUp _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = False }
		return []
	    processEvent (MouseButtonDown _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = True }
		return [ CmdSelect ]
	    processEvent (MouseButtonUp _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = False }
		return [ CmdUnselect ]
	    processEvent (MouseButtonDown _ _ ButtonWheelUp) = do
		rb <- gets rightButtonDown
		if rb
		    then return [ CmdRotate 1 ]
		    else do 
			modify $ \s -> s { paintTile = case paintTile s of
			    Just (BlockTile _) -> Just $ SpringTile Relaxed hu
			    Just (SpringTile _ _) -> Just $ PivotTile zero
			    Just (PivotTile _) -> Just $ ArmTile zero False
			    Just (ArmTile _ _) -> Nothing
			    Nothing -> Just $ BlockTile []
			    }
			return []
	    processEvent (MouseButtonDown _ _ ButtonWheelDown) = do
		rb <- gets rightButtonDown
		if rb
		    then return [ CmdRotate $ -1 ]
		    else do 
			modify $ \s -> s { paintTile = case paintTile s of
			    Just (BlockTile _) -> Nothing
			    Nothing -> Just $ ArmTile zero False
			    Just (SpringTile _ _) -> Just $ BlockTile []
			    Just (PivotTile _) -> Just $ SpringTile Relaxed hu
			    Just (ArmTile _ _) -> Just $ PivotTile zero
			    }
			return []
	    processEvent (VideoResize w h) = do
		initVideo w h
		return [ CmdRedraw ]
		
	    processEvent _ = return []
	    paintCmd (Just t) = CmdTile t
	    paintCmd Nothing = CmdWait
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
