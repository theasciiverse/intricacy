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
import Data.List (maximumBy, findIndex)
--import Debug.Trace (traceShow)

import UI
import Hex
import GameState
import Physics
import Command
import KeyBindings

maxlocksize = 12 -- TODO: use actual lock size?
periphery 0 = ((3*maxlocksize)`div`2)<*>hu <+> ((3*maxlocksize)`div`4)<*>hv
periphery n = rotate n $ periphery 0

-- aaPolygon seems to be a bit buggy in sdl-gfx-0.6.0
aaPolygon' surf verts col =
    aaLines surf (verts ++ take 1 verts) col
aaLines surf verts col =
    sequence_ [ aaLine surf x y x' y' col |
	((x,y),(x',y')) <- zip (take (length verts - 1) verts) (drop 1 verts) ]
	

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

tileGlyph (BlockTile adjs) col centre size surf = do
    filledPolygon surf corners col
    aaPolygon' surf corners $ bright col
    return ()
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
	    (sx,sy) = corner centre size (hextant dir - 1)
	    (offx,offy) = corner centre size (hextant dir)
	    (ex,ey) = corner centre size (hextant dir - 3)
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

tileGlyph BallTile col centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ 2*size`div`3) $ col
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ 2*size`div`3) $ bright col
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

blockedPush :: HexDir -> Pixel -> Glyph
blockedPush dir col centre size surf = do
    void $ aaLine surf fromx fromy tox toy col
    where
	(fromx, fromy) = innerCorner centre size dir
	(tox, toy) = edge centre size dir
	
collisionMarker :: Glyph
collisionMarker centre@(SVec x y) size surf = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`3) $ bright orange
    aaCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral $ size`div`3) $ bright orange
    return ()

hollowGlyph :: Pixel -> Glyph
hollowGlyph col centre size surf =
    aaPolygon' surf corners $ bright col
    where corners = [ innerCorner centre size dir | dir <- hexDirs ]

buttonGlyph :: Pixel -> Glyph
buttonGlyph = tileGlyph (BlockTile [])

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

ballCol = colourWheel 0

colourOf colouring idx = 
    case Map.lookup idx colouring of
	    Nothing -> white
	    Just n -> colourWheel n

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring highlight (owner,t) =
    let col = case t of
	    BallTile -> dim ballCol
	    _ -> colourOf colouring owner
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
    , dispFont::Maybe TTF.Font
    , lastFrameTicks::Word32, paintTile::Int
    , leftButtonDown::Bool, rightButtonDown::Bool
    , mousePos::HexVec
    , dispCentre::HexPos, dispLastCol::PieceColouring }
type UIM = StateT UIState IO
nullUIState = UIState 0 0 Nothing 0 0 False False zero (PHS zero) Map.empty

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

drawBlocked :: GameState -> PieceColouring -> Force -> UIM ()
drawBlocked st colouring (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
	    PlacedPiece pos (Pivot arms) -> (pos,arms)
	    PlacedPiece pos (Hook arm _) -> (pos,[arm])
	col = obscure $ colourOf colouring idx
    sequence_ [ drawAt (blockedArm arm dir col) (arm <+> pos) |
	arm <- arms ]
drawBlocked st colouring (Push idx dir) = do
    let footprint = fullFootprint st idx
	col = obscure $ colourOf colouring idx
    sequence_ [ drawAt (blockedPush dir col) pos
	| pos <- footprint
	, not $ (dir<+>pos) `elem` footprint ]

paintTiles :: [ Maybe Tile ]
paintTiles =
    [ Just $ BlockTile []
    , Just $ SpringTile Relaxed hu
    , Just $ PivotTile zero
    , Just $ ArmTile zero False
    , Just BallTile
    , Nothing
    ]

paintButtonStart :: HexVec
paintButtonStart = periphery 0 <+> (- length paintTiles `div` 2)<*>hv

drawPaintSel :: UIM ()
drawPaintSel = do
    pti <- gets paintTile
    case (paintTiles!!pti) of
	Nothing -> return ()
	Just t -> drawAtRel (tileGlyph t (dim $ colourWheel 5)) (periphery 0)

drawPaintButtons :: UIM ()
drawPaintButtons = do
    pti <- gets paintTile
    sequence_ [ 
	do
	    let gl = case paintTiles!!i of
		    Nothing -> hollowGlyph $ dim $ colourWheel 5
		    Just t -> tileGlyph t $ dim $ colourWheel 5
	    drawAtRel gl pos
	    when selected $ drawAtRel cursor pos
	| i <- [0..(length paintTiles - 1)]
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

drawButtons :: InputMode -> UIM ()
drawButtons mode = do
    sequence_ $ concat [
	    [ drawAtRel (buttonGlyph col) v >> renderStrAt [char] v |
		(i,(v,char)) <- enumerate $ map (\b->(buttonPos b, buttonChar b)) $ buttonGroup
		, let col = dim $ colourWheel (2*i) ]
	| buttonGroup <- buttons mode
	]
    where enumerate = zip [0..]

data Button = Button { buttonPos::HexVec, buttonCmd::Command, buttonChar::Char, buttonString::String }
buttons :: InputMode -> [ [ Button ] ]
buttons mode = case mode of 
    IMEdit -> [commontl ++
	[ Button (tl<+>hv) CmdResetTools 'T' "ResetTools"
	, Button (tl<+>(neg hw)) CmdPlay 'P' "Play"
	], [quitButton]]
    IMPlay -> [commontl, [quitButton]]
    IMConfirm -> [
	[ quitButton
	, Button (br<+>hw) CmdYes 'Y' "Yes"
	, Button (br<+>(neg hv)) CmdNo 'N' "No"
	] ]
    where
	commontl = [ Button tl CmdUndo 'U' "Undo" ]
	quitButton = Button br CmdQuit 'Q' "Quit"
	tl = periphery 2
	br = periphery 5


initVideo :: Int -> Int -> UIM ()
initVideo w h = do
    liftIO $ setVideoMode w h 32 [Resizable]
    modify $ \ds -> ds { scrWidth = w }
    modify $ \ds -> ds { scrHeight = h }
    (_,size) <- getGeom
    font <- liftIO $ TTF.tryOpenFont "VeraMono.ttf" (size)
    modify $ \ds -> ds { dispFont = font }

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawPlayState (PlayState (st:_) alerts wsel _) = do
	waitFrame
	let selTools = [ idx |
		(idx, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]
	drawState selTools False alerts st
	drawButtons IMPlay
	refresh

    drawEditState (EditState (st:_) selPiece selPos _ _) = do
	waitFrame
	drawState (maybeToList selPiece) True [] st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	-- drawPaintSel
	drawButtons IMEdit
	drawPaintButtons
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
		return True

    endUI = liftIO $ quit

    getInput mode = do
	events <- liftIO $ getEvents
	concat `liftM` (sequence $ map processEvent events)
	where
	    processEvent (KeyDown (Keysym _ _ ch)) = do
		let mCmd = Map.lookup ch $ bindings mode
		sequence_
		    [ modify $ \s -> s { paintTile = pti }
			| (pti,pt) <- zip [0..] paintTiles
			, (pt == Nothing && mCmd == Just CmdWait) ||
			    (isJust $ do
				pt' <- pt
				CmdTile t <- mCmd
				guard $ sameTileType t pt') ]
		return $ maybeToList mCmd
	    processEvent (MouseMotion _ _ _ _) = do
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
				pti <- gets paintTile
				lb <- gets leftButtonDown
				more <- mouseTo mPos
				return $ [ CmdDir WHSSelected dir ] ++
				    (if lb then [ drawCmd (paintTiles!!pti) True ] else []) ++
				    more
			    else return []
	    processEvent (MouseButtonDown _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = True }
		(mPos,central) <- getMousePos
		let bCmd = listToMaybe [ buttonCmd button
			| button <- concat $ buttons mode
			, mPos == buttonPos button
			, central
			]
		case bCmd of
		    Just c -> return [c]
		    Nothing -> do
			rb <- gets rightButtonDown
			if rb
			    then return [ CmdWait ]
			    else do
				let paintSel = listToMaybe [ i
					| i <- [0..(length paintTiles - 1)]
					, mPos == paintButtonStart <+> i<*>hv
					]
				case paintSel of
				    Just i -> do
					modify $ \s -> s { paintTile = i }
					return []
				    Nothing -> do
					pti <- gets paintTile
					return $ [ drawCmd (paintTiles!!pti) False ]
	    processEvent (MouseButtonUp _ _ ButtonLeft) = do
		modify $ \s -> s { leftButtonDown = False }
		return []
	    processEvent (MouseButtonDown _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = True }
		return [ CmdSelect ]
	    processEvent (MouseButtonUp _ _ ButtonRight) = do
		modify $ \s -> s { rightButtonDown = False }
		return [ CmdUnselect ]
	    processEvent (MouseButtonDown _ _ ButtonWheelUp) = doWheel 1
	    processEvent (MouseButtonDown _ _ ButtonWheelDown) = doWheel $ -1
	    processEvent (MouseButtonDown _ _ ButtonMiddle) = do
		rb <- gets rightButtonDown
		return $ if rb then [ CmdDelete ] else []
	    processEvent (VideoResize w h) = do
		initVideo w h
		return [ CmdRedraw ]
		
	    processEvent _ = return []

	    doWheel dw = do
		rb <- gets rightButtonDown
		if rb
		    then return [ CmdRotate dw ]
		    else do
			modify $ \s -> s { paintTile = (paintTile s + dw) `mod` (length paintTiles) }
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
