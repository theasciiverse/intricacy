module SDLRender where

import Graphics.UI.SDL
import Graphics.UI.SDL.Primitives
import qualified Graphics.UI.SDL.TTF as TTF
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Data.Map as Map
import Data.List (maximumBy)
import Data.Function (on)

import Hex
import GameState
import GameStateTypes
import BoardColouring
import Physics
import Command

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
    aaPolygon' surf verts $ opaquify rimCol
    return ()

rimmedCircle surf centre@(SVec x y) rad fillCol rimCol = do
    filledCircle surf (fromIntegral x) (fromIntegral y) (fromIntegral rad) fillCol
    aaCircle' surf (fromIntegral x) (fromIntegral y) (fromIntegral rad) $ opaquify rimCol
    return ()

type Glyph = SVec -> Int -> Surface -> IO ()

ysize :: Int -> Int
ysize size = round $ fromIntegral size / sqrt 3

corner :: Integral i => SVec -> Int -> Int -> (i,i)
corner (SVec x y) size hextant = (fromIntegral $ x+dx, fromIntegral $ y+dy)
    where
	[dx,dy] = f hextant
	f 0 = [size, -ysize size]
	f 1 = [0, -2*ysize size]
	f 2 = [-size, -ysize size]
	f n | n < 6 = let [x,y] = f (5-n) in [x,-y]
	    | n < 0 = f (6-n)
	    | otherwise = f (n`mod`6)

innerCorner :: Integral i => SVec -> Int -> HexDir -> (i,i)
innerCorner (SVec x y) size dir = (fromIntegral $ x+dx, fromIntegral $ y+dy)
    where
	[dx,dy] = f dir
	f dir
	    | dir == hu = [2*isize, 0]
	    | dir == hv = [-isize, -ysize size]
	    | dir == hw = [-isize, ysize size]
	    | not (isHexDir dir) = error "innerCorner: not a hexdir"
	    | otherwise = map (\z -> -z) $ f $ neg dir
	isize = size `div` 3

edge :: Integral i => SVec -> Int -> HexDir -> (i,i)
edge (SVec x y) size dir = (fromIntegral $ x+dx, fromIntegral $ y+dy)
    where
	[dx,dy] = f dir
	f dir
	    | dir == hu = [size, 0]
	    | dir == hv = [-size`div`2, -3*ysize size`div`2]
	    | dir == hw = [-size`div`2, 3*ysize size`div`2]
	    | not (isHexDir dir) = error "edge: not a hexdir"
	    | otherwise = map (\z -> -z) $ f $ neg dir

tileGlyph :: Tile -> Pixel -> Glyph

tileGlyph (BlockTile adjs) col centre size surf =
    rimmedPolygon surf corners col $ bright col
	where 
	    corners = concat [
		if or $ map adjAt [0,1]
		then [corner centre size $ hextant dir]
		else if adjAt $ -1
		    then []
		    else [innerCorner centre size dir]
		| dir <- hexDirs
		, let adjAt r = rotate r dir `elem` adjs
		]

tileGlyph (SpringTile extn dir) col centre size surf =
    aaLines surf points $ bright col
	where
	    n = 3*case extn of
		Stretched -> 1
		Relaxed -> 2
		Compressed -> 3
	    dir' = if dir == zero then hu else dir
	    (sx,sy) = corner centre size (hextant dir' - 1)
	    (offx,offy) = corner centre size (hextant dir')
	    (ex,ey) = corner centre size (hextant dir' - 3)
	    points = [ (x+dx,y+dy)
		| i <- [0..n]
		, i`mod`3 /= 1
		, let (x,y) = if i`mod`3==0 then (sx,sy) else (offx,offy)
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
	    (fx,fy) = innerCorner centre size $ neg mom
	    (tx,ty) = edge centre size $ neg mom
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
    void $ aaLine surf `uncurry` base `uncurry` tip $ col
    --void $ aaLine surf `uncurry` from' `uncurry` to' $ col
    void $ aaLine surf `uncurry` tip `uncurry` (arms!!0) $ col
    void $ aaLine surf `uncurry` tip `uncurry` (arms!!1) $ col
    where
	--base@(bx,by) = innerCorner centre size dir
	SVec bx' by' = centre
	base@(bx,by) = (fromIntegral bx',fromIntegral by')
	tip@(tx,hy) = edge centre size dir
	arms = [(bx + (tx-bx)`div`2 + dir*(hy-by)`div`4,
	    by + (hy-by)`div`2 - dir*(tx-bx)`div`4) | dir <- [-1,1]]

	--from' = corner centre size $ hextant dir
	--to' = corner centre size $ (hextant dir) - 1
	
collisionMarker :: Glyph
collisionMarker centre size surf =
    rimmedCircle surf centre (size`div`3) (bright orange) $ bright orange

hollowGlyph :: Pixel -> Glyph
hollowGlyph col centre size surf =
    aaPolygon' surf corners $ opaquify col
    where corners = map (corner centre size) [0..5]
hollowInnerGlyph col centre size surf =
    aaPolygon' surf corners $ opaquify col
    where corners = [ innerCorner centre size dir | dir <- hexDirs ]

filledHexGlyph :: Pixel -> Glyph
filledHexGlyph col centre size surf =
    rimmedPolygon surf corners col $ bright col
    where corners = map (corner centre size) [0..5]

buttonGlyph :: Pixel -> Glyph
buttonGlyph = tileGlyph (BlockTile [])

useFiveColourButton :: Bool -> Glyph
useFiveColourButton using centre size surf = do
    mapM_ (\h -> tileGlyph (BlockTile [])
	    (dim $ colourWheel (if using then h`div`2 else 1))
	    (SVec `uncurry` corner centre (size`div`2) h) (size`div`2) surf)
	[0,2,4]

showBlocksButton :: Bool -> Glyph
showBlocksButton showing centre size surf = do
    tileGlyph (BlockTile []) (dim red) centre size surf
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
cursor = hollowGlyph $ bright white

unfreshGlyph centre@(SVec x y) size surf = do
    let col = bright red
    hollowInnerGlyph col centre size surf
    sequence_ [pixel surf (fromIntegral $ x+(i*size`div`4)) (fromIntegral y) col
	| i <- [-1..1] ]

playerGlyph col centre size surf = filledHexGlyph col centre (size-1) surf

setPixelAlpha alpha (Pixel v) = Pixel $ v `div` 0x100 * 0x100 + alpha
bright = setPixelAlpha 0xff
brightish = setPixelAlpha 0xc0
dim = setPixelAlpha 0xa0
obscure = setPixelAlpha 0x80
faint = setPixelAlpha 0x40
invisible = setPixelAlpha 0x00

pixelToRGBA (Pixel v) =
    let (r,v') = divMod v 0x1000000
	(g,v'') = divMod v' 0x10000
	(b,a) = divMod v'' 0x100
    in (r,g,b,a)
rgbaToPixel (r,g,b,a) = Pixel $ a+0x100*(b+0x100*(g+0x100*r))
opaquify p =
    let (r,g,b,a) = pixelToRGBA p
	[r',g',b'] = map (\v -> (v*a)`div`0xff) [r,g,b]
    in rgbaToPixel (r',g',b',0xff)

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

red = colourWheel 0
yellow = colourWheel 1
green = colourWheel 2
cyan = colourWheel 3
blue = colourWheel 4
purple = colourWheel 5

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
    SVec ((x-z) * size) (-y * 3 * ysize size)

sVec2dHV :: Int -> SVec -> (Double,Double,Double)
sVec2dHV size (SVec sx sy) =
    let sx',sy',size' :: Double
	[sx',sy',size',ysize'] = map fromIntegral [sx,sy,size,ysize size]
	y' = -sy' / ysize' / 3
	x' = ((sx' / size') - y') / 2
	z' = -((sx' / size') + y') / 2
    in (x',y',z')

sVec2HexVec :: Int -> SVec -> HexVec
sVec2HexVec size sv =
    let (x',y',z') = sVec2dHV size sv
	unrounded = Map.fromList [(1,x'),(2,y'),(3,z')]
	rounded = Map.map round unrounded
	maxdiff = fst $ maximumBy (compare `on` snd) $
	    [ (i, abs $ c'-c) | i <- [1..3],
		let c' = unrounded Map.! i, let c = fromIntegral $ rounded Map.! i]
	[x,y,z] = map snd $ Map.toList $
	    Map.adjust (\x -> x - (sum $ Map.elems rounded)) maxdiff rounded
    in HexVec x y z

data RenderContext = RenderContext
    { renderSurf :: Surface
    , renderHCentre :: HexPos
    , renderSCentre :: SVec
    , renderSize :: Int
    , renderFont :: Maybe TTF.Font
    }
type RenderM = ReaderT RenderContext IO

displaceRender :: SVec -> RenderM a -> RenderM a
displaceRender disp = local displace
    where displace rc = rc { renderSCentre = renderSCentre rc <+> disp }

recentreAt :: HexVec -> RenderM a -> RenderM a
recentreAt v m = do
    size <- asks renderSize
    displaceRender (hexVec2SVec size v) m

rescaleRender :: RealFrac n => n -> RenderM a -> RenderM a
rescaleRender r = local resize
    where resize rc = rc { renderSize = round $ r * (fromIntegral $ renderSize rc) }

withFont :: Maybe TTF.Font -> RenderM a -> RenderM a
withFont font = local refont
    where refont rc = rc { renderFont = font }

erase :: RenderM ()
erase = do
    surf <- asks renderSurf
    void $ liftIO $ fillRect surf Nothing black


drawAt :: Glyph -> HexPos -> RenderM ()
drawAt gl pos = do
	centre <- asks renderHCentre
	drawAtRel gl (pos <-> centre)
drawAtRel gl v = do
	(surf, scrCentre, size) <- asks $ liftM3 (,,) renderSurf renderSCentre renderSize
	let cpos = scrCentre <+> (hexVec2SVec size v)
	liftIO $ gl cpos size surf

messageCol = white
accessedCol = green
dimWhiteCol = Pixel 0xa0a0a000
buttonTextCol = white
errorCol = red

pixelToColor p =
    let (r,g,b,_) = pixelToRGBA p
    in Color (fromIntegral r) (fromIntegral g) (fromIntegral b)

renderStrColAtLeft = renderStrColAt' False
renderStrColAt = renderStrColAt' True
renderStrColAt' :: Bool -> Pixel -> String -> HexVec -> RenderM ()
renderStrColAt' centred c str v = void $ runMaybeT $ do
    font <- MaybeT $ asks renderFont
    fsurf <- MaybeT $ liftIO $ TTF.tryRenderTextBlended font str $ pixelToColor c
    (surf, scrCentre, size) <- lift $ asks $
	liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec x y = scrCentre <+> (hexVec2SVec size v)
	    <+> neg (SVec 0 ((surfaceGetHeight fsurf)`div`2) <+>
		if centred
		    then SVec ((surfaceGetWidth fsurf)`div`2) 0
		    else SVec 0 0)
    void $ liftIO $ blitSurface fsurf Nothing surf (Just $ Rect x y 0 0)

blankRow v = do
    (surf, scrCentre, size) <- asks $
	liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec _ y = scrCentre <+> (hexVec2SVec size v)
	w = surfaceGetWidth surf
	h = ceiling $ fromIntegral size * 2 * sqrt 3
    void $ liftIO $ fillRect surf (Just $
	Rect 0 (y-h`div`2)
	    w h
	) black

drawCursorAt :: Maybe HexPos -> RenderM ()
drawCursorAt (Just pos) = drawAt cursor pos
drawCursorAt _ = return ()

drawBlocked :: GameState -> PieceColouring -> Bool -> Force -> RenderM ()
drawBlocked st colouring blocking (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
	    PlacedPiece pos (Pivot arms) -> (pos,arms)
	    PlacedPiece pos (Hook arm _) -> (pos,[arm])
	    _ -> (pos,[])
	col = if blocking then bright $ purple else dim $ colourOf colouring idx
    sequence_ [ drawAt (blockedArm arm dir col) (arm <+> pos) |
	arm <- arms ]
drawBlocked st colouring blocking (Push idx dir) = do
    let footprint = plPieceFootprint $ getpp st idx
	fullfootprint = fullFootprint st idx
	col = bright $ if blocking then purple else orange
    sequence_ [ drawAt (blockedPush dir col) pos
	| pos <- footprint
	, (dir<+>pos) `notElem` fullfootprint ]
    -- drawAt (blockedPush dir $ bright orange) $ placedPos $ getpp st idx

blitAt :: Surface -> HexVec -> RenderM ()
blitAt surface v = do
    (surf, scrCentre, size) <- asks $ liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec x y = scrCentre <+> (hexVec2SVec size v)
	w = surfaceGetWidth surface
	h = surfaceGetHeight surface
    void $ liftIO $ blitSurface surface Nothing surf $ Just $
	Rect (x-w`div`2) (y-h`div`2) (w+1) (h+1)
