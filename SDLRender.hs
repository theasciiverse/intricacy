-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module SDLRender where

import Graphics.UI.SDL
import Graphics.UI.SDL.Primitives
import qualified Graphics.UI.SDL.TTF as TTF
import Data.Monoid
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List (maximumBy)
import Data.Function (on)
import GHC.Int (Int16)
import Control.Applicative
import System.Random (randomRIO)

import Hex
import GameState
import GameStateTypes
import BoardColouring
import Physics
import Command
import Util

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

circleAt surf centre@(SVec x y) rad col =
    aaCircle' surf (fi x) (fi y) (fi rad) col

filledCircleAt surf centre@(SVec x y) rad col =
    filledCircle surf (fi x) (fi y) (fi rad) col

rimmedCircle surf centre@(SVec x y) rad fillCol rimCol = void $ do
    filledCircleAt surf centre rad fillCol
    circleAt surf centre rad $ opaquify rimCol

thickLine :: Surface -> (Int16,Int16) -> (Int16,Int16) -> Float -> Pixel -> IO ()
thickLine surf from@(x,y) to@(x',y') thickness col = do
    let (dx,dy) = (x'-x,y'-y)
	[rdx,rdy] = map fi [dx,dy]
	s = thickness / (sqrt $ rdx^2 + rdy^2)
	perp@(px,py) = (round $ s*rdy, round $ s*(-rdx))
	mperp = (-px,-py)
	addHalf (a,b) (c,d) = ( (2*a + c) `div` 2, (2*b + d) `div` 2 )
    rimmedPolygon surf
	(map (uncurry addHalf) [(from,perp),(to,perp),(from,mperp),(to,mperp)])
	(dim col) (bright col)

thickLines surf verts thickness col =
    sequence_ [ thickLine surf v v' thickness col |
	(v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

thickPolygon surf verts thickness col =
    thickLines surf (verts ++ take 1 verts) thickness col

ysize :: Int -> Int
ysize = (map (\size -> round $ fi size / sqrt 3) [0..] !!)

corner :: Integral i => SVec -> Int -> Int -> (i,i)
corner (SVec x y) size hextant = (fi $ x+dx, fi $ y+dy)
    where
	[dx,dy] = f hextant
	f 0 = [size, -ysize size]
	f 1 = [0, -2*ysize size]
	f 2 = [-size, -ysize size]
	f n | n < 6 = let [x,y] = f (5-n) in [x,-y]
	    | n < 0 = f (6-n)
	    | otherwise = f (n`mod`6)

innerCorner :: Integral i => SVec -> Int -> HexDir -> (i,i)
innerCorner (SVec x y) size dir = (fi $ x+dx, fi $ y+dy)
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
edge (SVec x y) size dir = (fi $ x+dx, fi $ y+dy)
    where
	[dx,dy] = f dir
	f dir
	    | dir == hu = [size, 0]
	    | dir == hv = [-size`div`2, -3*ysize size`div`2]
	    | dir == hw = [-size`div`2, 3*ysize size`div`2]
	    | not (isHexDir dir) = error "edge: not a hexdir"
	    | otherwise = map (\z -> -z) $ f $ neg dir

data ShowBlocks = ShowBlocksBlocking | ShowBlocksAll | ShowBlocksNone
    deriving (Eq, Ord, Show, Read)

data Glyph
    = TileGlyph Tile Pixel
    | BlockedArm HexDir TorqueDir Pixel
    | TurnedArm HexDir TorqueDir Pixel
    | BlockedBlock Tile HexDir Pixel
    | BlockedPush HexDir Pixel
    | CollisionMarker
    | HollowGlyph Pixel
    | HollowInnerGlyph Pixel
    | FilledHexGlyph Pixel
    | ButtonGlyph Pixel
    | UseFiveColourButton Bool
    | ShowBlocksButton ShowBlocks
    | ShowButtonTextButton Bool
    | UseSoundsButton Bool
    | WhsButtonsButton (Maybe WrHoSel)
    | FullscreenButton Bool
    | UnfreshGlyph
    deriving (Eq, Ord, Show)

type SizedGlyph = (Glyph,Int)
data CachedGlyphs = CachedGlyphs (Map SizedGlyph Surface) [SizedGlyph]
    deriving (Eq, Ord, Show)
emptyCachedGlyphs = CachedGlyphs Map.empty []
maxCachedGlyphs = 100

renderGlyphCaching :: Glyph -> SVec -> Int -> Surface -> RenderM ()
-- Glyph caching:
--  We aim to cache glyphs which are "currently" being regularly drawn, so
--  they can be blitted from RAM rather than being drawn afresh each time.
--  Rather than track statistics, we adopt the following probabilistic scheme.
renderGlyphCaching gl centre size surf = do
    CachedGlyphs cmap clist <- lift get
    let cacheFull = Map.size cmap >= maxCachedGlyphs
    let mcsurf = Map.lookup sgl cmap
    -- with probability 1 in (maxCachedGlyphs`div`2), we put this glyph at the
    -- head of the cached list, throwing away the tail to make room if needed.
    cacheIt <- (((cacheable &&) . (not cacheFull ||)) <$>) $
	liftIO $ (==0) <$> randomRIO (0::Int,maxCachedGlyphs`div`2)
    case mcsurf of
	Nothing -> if cacheIt
	    then do
		csurf <- newGlyphSurf
		renderOnCache csurf
		addToCache cacheFull csurf
		blitGlyph csurf
	    else
		liftIO $ renderGlyph gl centre size surf
	Just csurf -> do
	    when cacheIt promote
	    blitGlyph csurf
    where
	sgl = (gl,size)
	cacheable = case gl of
	    -- some glyphs need to be drawn with blending - those involving
	    -- anti-aliasing which bleed over the edge of the hex or which
	    -- may be drawn on top of an existing glyph.
	    -- TODO: we should find a way to deal with at least some of these;
	    -- springs in particular are common and expensive to draw.
	    -- Maybe we could truncate the spring glyphs to a hex?
	    TileGlyph (BlockTile adjs) _ -> null adjs
	    TileGlyph (SpringTile extn dir) _ -> False
	    FilledHexGlyph _ -> False
	    HollowGlyph _ -> False
	    BlockedBlock _ _ _ -> False
	    BlockedPush _ _ -> False
	    CollisionMarker -> False
	    _ -> True
	w = size*2 + 1
	h = ysize size*4 + 1
	newGlyphSurf = do
	    -- csurf <- liftIO $ createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
	    csurf <- liftIO $ createRGBSurface [] w h 16 0 0 0 0
	    liftIO $ setColorKey csurf [SrcColorKey,RLEAccel] $ Pixel 0
	    return csurf
	renderOnCache csurf =
	    liftIO $ renderGlyph gl (SVec (w`div`2) (h`div`2)) size csurf
	addToCache cacheFull csurf = do
	    CachedGlyphs cmap clist <- lift get
	    let cmap' = Map.insert sgl csurf cmap
	    lift $ put $ if cacheFull
		then CachedGlyphs (Map.delete (last clist) cmap') (sgl:List.init clist)
		else CachedGlyphs cmap' (sgl:clist)
	promote = do
	    CachedGlyphs cmap clist <- lift get
	    lift $ put $ CachedGlyphs cmap (sgl:List.delete sgl clist)
	blitGlyph csurf =
	    let SVec x y = centre
	    in void $ liftIO $ blitSurface csurf Nothing surf $ Just $
		Rect (x-w`div`2) (y-h`div`2) (w+1) (h+1)

renderGlyph :: Glyph -> SVec -> Int -> Surface -> IO ()
renderGlyph (TileGlyph (BlockTile adjs) col) centre size surf =
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

renderGlyph (TileGlyph (SpringTile extn dir) col) centre size surf =
    thickLines surf points 1 $ brightness col
	where
	    n = 3*case extn of
		Stretched -> 1
		Relaxed -> 2
		Compressed -> 4
	    brightness = if extn == Relaxed then dim else bright
	    dir' = if dir == zero then hu else dir
	    (sx,sy) = corner centre size (hextant dir' - 1)
	    (offx,offy) = corner centre size (hextant dir')
	    (ex,ey) = corner centre size (hextant dir' - 3)
	    points = [ (x+dx,y+dy)
		| i <- [0..n]
		, i`mod`3 /= 1
		, let (x,y) = if i`mod`3==0 then (sx,sy) else (offx,offy)
		, let (dx,dy) = ((i*(ex-sx))`div`n, (i*(ey-sy))`div`n) ]

renderGlyph (TileGlyph (PivotTile dir) col) centre size surf = do
    rimmedCircle surf centre rad col $ bright col
    when (dir /= zero)
	$ void $ aaLine surf `uncurry` from `uncurry` to $ bright col
    return ()
	where
	    rad = (7*size)`div`8
	    from = edge centre rad $ neg dir
	    to = edge centre rad dir

renderGlyph (TileGlyph (ArmTile dir _) col) centre size surf =
    void $ thickLine surf from to 1 col
	where
	    dir' = if dir == zero then hu else dir
	    from = edge centre size $ neg dir'
	    to = innerCorner centre size dir'

renderGlyph (TileGlyph HookTile col) centre size surf =
    rimmedCircle surf centre rad col $ bright col
	where
	    rad = (7*size)`div`8

renderGlyph (TileGlyph (WrenchTile mom) col) centre size surf = do
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

renderGlyph (TileGlyph BallTile col) centre size surf =
    rimmedCircle surf centre rad (faint col) (obscure col)
    where rad = (7*size)`div`8

renderGlyph (BlockedArm armdir tdir col) centre size surf =
    void $ aaLine surf `uncurry` from `uncurry` to $ col
	where
	    from = innerCorner centre size $ rotate (2*tdir) armdir
	    to = edge centre size $ rotate tdir armdir

renderGlyph (TurnedArm armdir tdir col) centre size surf =
    sequence_ [ arc surf (fi x) (fi y) (fi $ n*size `div` 4)
	a1 a2 col | n <- [8,9] ]
    where
	SVec x y = centre +^ hexVec2SVec size (neg armdir)
	a0 = fi $ -60*hextant armdir
	a1' = a0 + fi tdir * 10
	a2' = a0 + fi tdir * 30
	a1 = min a1' a2'
	a2 = max a1' a2'

renderGlyph (BlockedBlock tile dir col) centre size surf =
    renderGlyph (TileGlyph tile col) (shift +^ centre) size surf
	where
	    shift = SVec (x'-x) (y'-y)
	    (x,y) = innerCorner centre size dir
	    (x',y') = edge centre size dir

renderGlyph (BlockedPush dir col) centre size surf = do
{-
    void $ rimmedPolygon surf verts (obscure col) (dim col)
	where verts =
		[ innerCorner centre size $ rotate 1 dir
		, innerCorner centre size $ rotate (-1) dir
		, innerCorner centre size dir ]
-}
    void $ thickLine surf base tip 1 col
    --void $ aaLine surf `uncurry` from' `uncurry` to' $ col
    void $ thickLine surf tip (arms!!0) 1 col
    void $ thickLine surf tip (arms!!1) 1 col
    where
	--base@(bx,by) = innerCorner centre size dir
	SVec bx' by' = centre
	base@(bx,by) = (fi bx',fi by')
	tip@(tx,hy) = edge centre size dir
	arms = [(bx + (tx-bx)`div`2 + dir*(hy-by)`div`4,
	    by + (hy-by)`div`2 - dir*(tx-bx)`div`4) | dir <- [-1,1]]

	--from' = corner centre size $ hextant dir
	--to' = corner centre size $ (hextant dir) - 1

renderGlyph CollisionMarker centre@(SVec x y) size surf = void $ do
    -- rimmedCircle surf centre (size`div`3) (bright purple) $ bright purple
    aaLine surf `uncurry` start `uncurry` end $ col
    circleAt surf centre rad col
    where
	[start,end] = map (corner centre (size`div`2)) [0,3]
	rad = ysize size
	col = dim purple

renderGlyph (HollowGlyph col) centre size surf =
    aaPolygon' surf corners $ opaquify col
    where corners = map (corner centre size) [0..5]
renderGlyph (HollowInnerGlyph col) centre size surf =
    aaPolygon' surf corners $ opaquify col
    where corners = [ innerCorner centre size dir | dir <- hexDirs ]

renderGlyph (FilledHexGlyph col) centre size surf =
    rimmedPolygon surf corners col $ brightish col
    where corners = map (corner centre size) [0..5]

renderGlyph (ButtonGlyph col) centre size surf =
    renderGlyph (TileGlyph (BlockTile []) col) centre size surf

renderGlyph (UseFiveColourButton using) centre size surf = do
    mapM_ (\h -> renderGlyph (TileGlyph (BlockTile [])
	    (dim $ colourWheel (if using then h`div`2 else 1)))
	    (SVec `uncurry` corner centre (size`div`2) h) (size`div`2) surf)
	[0,2,4]

renderGlyph (ShowBlocksButton showing) centre size surf = do
    renderGlyph (TileGlyph (BlockTile []) (dim red)) centre size surf
    when (showing == ShowBlocksAll) $
	renderGlyph (BlockedPush hu (bright orange)) centre size surf
    when (showing /= ShowBlocksNone) $
	renderGlyph (BlockedPush hw (bright purple)) centre size surf

renderGlyph (ShowButtonTextButton showing) centre@(SVec x y) size surf = do
    renderGlyph (ButtonGlyph (dim yellow)) (SVec `uncurry` edge centre (size`div`2) (neg hu)) (size`div`2) surf
    when showing $
	sequence_ [ pixel surf (fi $ x+size`div`3+(i*size`div`4)) (fi $ y - size`div`4) (bright white)
	    | i <- [-1..1] ]

renderGlyph (UseSoundsButton use) centre@(SVec x y) size surf = do
    sequence [ arc surf (fi $ x - (2*size`div`3)) (fi y) r (-20) 20
            (if use then bright green else dim red)
        | r <- map fi $ map (*(size`div`3)) [1,2,3] ]
    when (not use) $ void $ aaLine surf
        `uncurry` (innerCorner centre size hw)
        `uncurry` (innerCorner centre size $ neg hw) $ dim red

renderGlyph (WhsButtonsButton Nothing) centre size surf =
    renderGlyph (ButtonGlyph (dim red)) centre (size`div`3) surf
    >> sequence_ [ renderGlyph (ButtonGlyph (dim purple))
	(SVec `uncurry` edge centre (size`div`2) dir) (size`div`3) surf
	| dir <- hexDirs ]
renderGlyph (WhsButtonsButton (Just whs)) centre size surf = do
    when (whs /= WHSHook) $
	renderGlyph (TileGlyph (WrenchTile zero) col) (miniCentre 0) miniSize surf
    when (whs /= WHSWrench) $ do
	renderGlyph (TileGlyph HookTile col) (miniCentre 4) miniSize surf
	renderGlyph (TileGlyph (ArmTile hv False) col) (miniCentre 2) miniSize surf
    where
	miniSize = size `div` 2
	miniCentre h = SVec `uncurry` corner centre miniSize h
	col = dim white

renderGlyph (FullscreenButton fs) centre size surf = do
    thickPolygon surf corners 1 $ activeCol (not fs)
    thickPolygon surf corners' 1 $ activeCol fs
    where
	activeCol True = opaquify $ dim green
	activeCol False = opaquify $ dim red
	size' = (2*size`div`3)
	corners = [ (if dir `elem` [hu,neg hu] then edge else innerCorner) centre size' dir
	    | dir <- hexDirs ]
	corners' = map (corner centre size') [0..5]

renderGlyph (UnfreshGlyph) centre@(SVec x y) size surf = do
    let col = bright red
    renderGlyph (HollowInnerGlyph col) centre size surf
    sequence_ [pixel surf (fi $ x+(i*size`div`4)) (fi y) col
	| i <- [-1..1] ]

playerGlyph col = FilledHexGlyph col

cursorGlyph = HollowGlyph $ bright white

ownedTileGlyph colouring highlight (owner,t) =
    let col = colourOf colouring owner
    in TileGlyph t $ (if owner `elem` highlight then bright else dim) col

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
	[sx',sy',size',ysize'] = map fi [sx,sy,size,ysize size]
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
		let c' = unrounded Map.! i, let c = fi $ rounded Map.! i]
	[x,y,z] = map snd $ Map.toList $
	    Map.adjust (\x -> x - (sum $ Map.elems rounded)) maxdiff rounded
    in HexVec x y z

data RenderContext = RenderContext
    { renderSurf :: Surface
    , renderBGSurf :: Maybe Surface
    , renderHCentre :: HexPos
    , renderSCentre :: SVec
    , renderSize :: Int
    , renderFont :: Maybe TTF.Font
    }
type RenderM = ReaderT RenderContext (StateT CachedGlyphs IO)

runRenderM :: RenderM a -> CachedGlyphs -> RenderContext -> IO (a,CachedGlyphs)
runRenderM m cgs rc = runStateT (runReaderT m rc) cgs

displaceRender :: SVec -> RenderM a -> RenderM a
displaceRender disp = local displace
    where displace rc = rc { renderSCentre = renderSCentre rc +^ disp }

recentreAt :: HexVec -> RenderM a -> RenderM a
recentreAt v m = do
    size <- asks renderSize
    displaceRender (hexVec2SVec size v) m

rescaleRender :: RealFrac n => n -> RenderM a -> RenderM a
rescaleRender r = local resize
    where resize rc = rc { renderSize = round $ r * (fi $ renderSize rc) }

withFont :: Maybe TTF.Font -> RenderM a -> RenderM a
withFont font = local refont
    where refont rc = rc { renderFont = font }

erase :: RenderM ()
erase = fillRectBG Nothing

fillRectBG :: Maybe Rect -> RenderM ()
fillRectBG mrect = do
    surf <- asks renderSurf
    mbgsurf <- asks renderBGSurf
    void $ liftIO $ maybe
	(fillRect surf mrect black)
	(\bgsurf -> blitSurface bgsurf mrect surf mrect)
	mbgsurf

drawBasicBG :: Int -> RenderM ()
drawBasicBG maxR = sequence_ [ drawAtRel (HollowGlyph $ colAt v) v | v <- hexDisc maxR ]
    where
	colAt v@(HexVec hx hy hz) = let
		[r,g,b] = map (\h -> fi $ ((0xff*)$ 5 + abs h)`div`maxR) [hx,hy,hz]
		a = fi $ (0x90 * (maxR - abs (hexLen v)))`div`maxR
	    in rgbaToPixel (r,g,b,a)

drawAt :: Glyph -> HexPos -> RenderM ()
drawAt gl pos = do
	centre <- asks renderHCentre
	drawAtRel gl (pos -^ centre)

drawAtRel :: Glyph -> HexVec -> RenderM ()
drawAtRel gl v = do
	(surf, scrCentre, size) <- asks $ liftM3 (,,) renderSurf renderSCentre renderSize
	let cpos = scrCentre +^ (hexVec2SVec size v)
	renderGlyphCaching gl cpos size surf

messageCol = white
dimWhiteCol = Pixel 0xa0a0a000
buttonTextCol = white
errorCol = red

pixelToColor p =
    let (r,g,b,_) = pixelToRGBA p
    in Color (fi r) (fi g) (fi b)

renderStrColAtLeft = renderStrColAt' False
renderStrColAt = renderStrColAt' True
renderStrColAt' :: Bool -> Pixel -> String -> HexVec -> RenderM ()
renderStrColAt' centred c str v = void $ runMaybeT $ do
    font <- MaybeT $ asks renderFont
    fsurf <- MaybeT $ liftIO $ TTF.tryRenderTextBlended font str $ pixelToColor c
    (surf, scrCentre, size) <- lift $ asks $
	liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec x y = scrCentre +^ (hexVec2SVec size v)
	    +^ neg (SVec 0 ((surfaceGetHeight fsurf-1)`div`2) +^
		if centred
		    then SVec ((surfaceGetWidth fsurf)`div`2) 0
		    else SVec 0 0)
    void $ liftIO $ blitSurface fsurf Nothing surf (Just $ Rect x y 0 0)

renderStrColAbove = renderStrColVShifted True
renderStrColBelow = renderStrColVShifted False
renderStrColVShifted :: Bool -> Pixel -> String -> HexVec -> RenderM ()
renderStrColVShifted up c str v = do
    size <- asks renderSize
    displaceRender (SVec size 0) $ renderStrColAt c str $ v +^ (if up then hv else hw)

blankRow v = do
    (surf, scrCentre, size) <- asks $
	liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec _ y = scrCentre +^ (hexVec2SVec size v)
	w = surfaceGetWidth surf
	h = ceiling $ fi (size * 3 `div` 2) * 2 / sqrt 3
    fillRectBG $ Just $ Rect 0 (y-h`div`2) w h

drawCursorAt :: Maybe HexPos -> RenderM ()
drawCursorAt (Just pos) = drawAt cursorGlyph pos
drawCursorAt _ = return ()

drawBlocked :: GameState -> PieceColouring -> Bool -> Force -> RenderM ()
drawBlocked st colouring blocking (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
	    PlacedPiece pos (Pivot arms) -> (pos,arms)
	    PlacedPiece pos (Hook arm _) -> (pos,[arm])
	    _ -> (pos,[])
	col = if blocking then bright $ purple else dim $ colourOf colouring idx
    sequence_ [ drawAt (BlockedArm arm dir col) (arm +^ pos) |
	arm <- arms ]
drawBlocked st colouring blocking (Push idx dir) = do
    let footprint = plPieceFootprint $ getpp st idx
	fullfootprint = fullFootprint st idx
	col = bright $ if blocking then purple else orange
    sequence_ [ drawAt (BlockedPush dir col) pos
	| pos <- footprint
	, (dir+^pos) `notElem` fullfootprint ]
    -- drawAt (blockedPush dir $ bright orange) $ placedPos $ getpp st idx

drawApplied :: GameState -> PieceColouring -> Force -> RenderM ()
drawApplied st colouring (Torque idx dir) = do
    let (pos,arms) = case getpp st idx of
	    PlacedPiece pos (Pivot arms) -> (pos,arms)
	    PlacedPiece pos (Hook arm _) -> (pos,[arm])
	    _ -> (pos,[])
	col = dim $ colourOf colouring idx
    sequence_ [ drawAt (TurnedArm arm dir col) (arm +^ pos) |
	    arm <- arms ]
drawApplied _ _ _ = return ()


blitAt :: Surface -> HexVec -> RenderM ()
blitAt surface v = do
    (surf, scrCentre, size) <- asks $ liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec x y = scrCentre +^ (hexVec2SVec size v)
	w = surfaceGetWidth surface
	h = surfaceGetHeight surface
    void $ liftIO $ blitSurface surface Nothing surf $ Just $
	Rect (x-w`div`2) (y-h`div`2) (w+1) (h+1)
