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

-- wrappers around sdl-gfx functions
pixelR v col = do
    (x,y) <- renderPos v
    surf <- asks renderSurf
    void.liftIO $ pixel surf x y col

aaLineR v v' col = do
    (x,y) <- renderPos v
    (x',y') <- renderPos v'
    surf <- asks renderSurf
    void.liftIO $ aaLine surf x y x' y' col

filledPolygonR verts fillCol = do
    ps <- mapM renderPos verts
    surf <- asks renderSurf
    void.liftIO $ filledPolygon surf ps fillCol

arcR v rad a1 a2 col = do
    (x,y) <- renderPos v
    r <- renderLen rad
    surf <- asks renderSurf
    void.liftIO $ arc surf x y r a1 a2 col

filledCircleR v rad col = do
    (x,y) <- renderPos v
    r <- renderLen rad
    surf <- asks renderSurf
    void.liftIO $ filledCircle surf x y r col

-- aaPolygon seems to be a bit buggy in sdl-gfx-0.6.0
aaPolygonR verts col =
    aaLinesR (verts ++ take 1 verts) col

-- aaCircle too
aaCircleR v rad col = do
    (x,y) <- renderPos v
    r <- renderLen rad
    surf <- asks renderSurf
    if (rad <= 1) then void.liftIO $ pixel surf x y col
    else void.liftIO $ aaCircle surf x y r col


aaLinesR verts col =
    sequence_ [ aaLineR v v' col |
	(v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

rimmedPolygonR verts fillCol rimCol = do
    filledPolygonR verts fillCol
    aaPolygonR verts $ opaquify rimCol
    return ()

rimmedCircleR v rad fillCol rimCol = void $ do
    filledCircleR v rad fillCol
    aaCircleR v rad $ opaquify rimCol

thickLineR :: FVec -> FVec -> Float -> Pixel -> RenderM ()
thickLineR from to thickness col =
    let FVec dx dy = to -^ from
        baseThickness = (1/16)
	s = baseThickness * thickness / (sqrt $ dx^2 + dy^2)
	perp = (s/2) **^ FVec dy (-dx)
    in rimmedPolygonR
        [ from +^ perp, to +^ perp
        , to +^ neg perp, from +^ neg perp]
	(dim col) (bright col)

thickLinesR verts thickness col =
    sequence_ [ thickLineR v v' thickness col |
	(v,v') <- zip (take (length verts - 1) verts) (drop 1 verts) ]

thickPolygonR verts thickness col =
    thickLinesR (verts ++ take 1 verts) thickness col

ylen = 1 / sqrt 3
ysize :: Int -> Int
ysize = (map (\size -> round $ fi size * ylen) [0..] !!)

corner :: Int -> FVec
corner hextant = FVec x y
    where
	[x,y] = f hextant
	f 0 = [1, -ylen]
	f 1 = [0, -2*ylen]
	f 2 = [-1, -ylen]
	f n | n < 6 = let [x,y] = f (5-n) in [x,-y]
	    | n < 0 = f (6-n)
	    | otherwise = f (n`mod`6)

innerCorner :: HexDir -> FVec
innerCorner dir = FVec x y
    where
	[x,y] = f dir
	f dir
	    | dir == hu = [2/3, 0]
	    | dir == hv = [-1/3, -ylen]
	    | dir == hw = [-1/3, ylen]
	    | not (isHexDir dir) = error "innerCorner: not a hexdir"
	    | otherwise = map (\z -> -z) $ f $ neg dir

edge :: HexDir -> FVec
edge dir = FVec x y
    where
	[x,y] = f dir
	f dir
	    | dir == hu = [1, 0]
	    | dir == hv = [-1/2, -3*ylen/2]
	    | dir == hw = [-1/2, 3*ylen/2]
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

renderGlyphCaching :: Glyph -> RenderM ()
-- Glyph caching:
--  We aim to cache glyphs which are "currently" being regularly drawn, so
--  they can be blitted from RAM rather than being drawn afresh each time.
--  Rather than track statistics, we adopt the following probabilistic scheme.
renderGlyphCaching gl = do
    CachedGlyphs cmap clist <- lift get
    size <- asks renderSize
    let sgl = (gl,size)
	w = size*2 + 1
	h = ysize size*4 + 1
	newGlyphSurf = do
	    -- csurf <- liftIO $ createRGBSurface [] w h 32 0xff000000 0x00ff0000 0x0000ff00 0x000000ff
	    csurf <- liftIO $ createRGBSurface [] w h 16 0 0 0 0
	    liftIO $ setColorKey csurf [SrcColorKey,RLEAccel] $ Pixel 0
	    return csurf
	renderOnCache csurf =
            let ccxt rc = rc { renderSurf = csurf, renderSCentre = SVec (w`div`2) (h`div`2) }
	    in local ccxt $ renderGlyph gl
	addToCache cacheFull csurf = do
	    CachedGlyphs cmap clist <- lift get
	    let cmap' = Map.insert sgl csurf cmap
	    lift $ put $ if cacheFull
		then CachedGlyphs (Map.delete (last clist) cmap') (sgl:List.init clist)
		else CachedGlyphs cmap' (sgl:clist)
	promote = do
	    CachedGlyphs cmap clist <- lift get
	    lift $ put $ CachedGlyphs cmap (sgl:List.delete sgl clist)
	blitGlyph csurf = do
            surf <- asks renderSurf
	    (x,y) <- renderPos zero
	    void $ liftIO $ blitSurface csurf Nothing surf $ Just $
		Rect (x-w`div`2) (y-h`div`2) (w+1) (h+1)
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
		renderGlyph gl
	Just csurf -> do
	    when cacheIt promote
	    blitGlyph csurf
    where
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

renderGlyph :: Glyph -> RenderM ()
renderGlyph (TileGlyph (BlockTile adjs) col) =
    rimmedPolygonR corners col $ bright col
	where
	    corners = concat [
		if or $ map adjAt [0,1]
		then [corner $ hextant dir]
		else if adjAt $ -1
		    then []
		    else [innerCorner dir]
		| dir <- hexDirs
		, let adjAt r = rotate r dir `elem` adjs
		]

renderGlyph (TileGlyph (SpringTile extn dir) col) =
    thickLinesR points 1 $ brightness col
	where
            n :: Int
	    n = 3*case extn of
		Stretched -> 1
		Relaxed -> 2
		Compressed -> 4
	    brightness = if extn == Relaxed then dim else bright
	    dir' = if dir == zero then hu else dir
	    s = corner (hextant dir' - 1)
	    off = corner (hextant dir')
	    e = corner (hextant dir' - 3)
	    points = [ b +^ (fi i / fi n) **^ (e -^ s)
		| i <- [0..n]
		, i`mod`3 /= 1
		, let b = if i`mod`3==0 then s else off ]

renderGlyph (TileGlyph (PivotTile dir) col) = do
    rimmedCircleR zero (7/8) col $ bright col
    when (dir /= zero)
	$ aaLineR from to $ bright col
    return ()
	where
	    from = (7/8) **^ edge (neg dir)
	    to = (7/8) **^ edge dir

renderGlyph (TileGlyph (ArmTile dir _) col) =
    thickLineR from to 1 col
	where
	    dir' = if dir == zero then hu else dir
	    from = edge $ neg dir'
	    to = innerCorner dir'

renderGlyph (TileGlyph HookTile col) =
    rimmedCircleR zero (7/8) col $ bright col

renderGlyph (TileGlyph (WrenchTile mom) col) = do
    rimmedCircleR zero (1/3) col $ bright col
    when (mom /= zero) $
	let
	    from = innerCorner $ neg mom
	    to = edge $ neg mom
	    shifts = [ (1/2) **^ (b -^ a)
		| rot <- [-1,0,1]
		, let a = innerCorner $ neg mom
		, let b = innerCorner $ rotate rot $ neg mom
		]
	in sequence_
	    [ aaLineR (from+^shift) (to+^shift) $ col
	    | shift <- shifts ]

renderGlyph (TileGlyph BallTile col) =
    rimmedCircleR zero (7/8) (faint col) (obscure col)

renderGlyph (BlockedArm armdir tdir col) =
    aaLineR from to col
	where
	    from = innerCorner $ rotate (2*tdir) armdir
	    to = edge $ rotate tdir armdir

renderGlyph (TurnedArm armdir tdir col) =
    sequence_ [ arcR c r a1 a2 col | r <- [8/4,9/4] ]
    where
	c = hexVec2FVec $ neg armdir
	a0 = fi $ -60*hextant armdir
	a1' = a0 + fi tdir * 10
	a2' = a0 + fi tdir * 30
	a1 = min a1' a2'
	a2 = max a1' a2'

renderGlyph (BlockedBlock tile dir col) =
    displaceRender shift $ renderGlyph (TileGlyph tile col)
	where shift = innerCorner dir -^ edge dir

renderGlyph (BlockedPush dir col) = do
    thickLineR zero tip 1 col
    thickLineR tip (arms!!0) 1 col
    thickLineR tip (arms!!1) 1 col
    where
	tip@(FVec tx ty) = edge dir
        arms = [ FVec ((tx/2) + d*ty/4) (ty/2 - d*tx/4) | d <- [-1,1] ]

renderGlyph CollisionMarker = do
    -- rimmedCircle surf centre (size`div`3) (bright purple) $ bright purple
    aaLineR start end $ col
    aaCircleR zero rad col
    where
	[start,end] = map (((1/2)**^) . corner) [0,3]
	rad = ylen
	col = dim purple

renderGlyph (HollowGlyph col) =
    aaPolygonR corners $ opaquify col
    where corners = map corner [0..5]
renderGlyph (HollowInnerGlyph col) =
    aaPolygonR corners $ opaquify col
    where corners = map innerCorner hexDirs

renderGlyph (FilledHexGlyph col) =
    rimmedPolygonR corners col $ brightish col
    where corners = map corner [0..5]

renderGlyph (ButtonGlyph col) =
    renderGlyph (TileGlyph (BlockTile []) col)

renderGlyph (UseFiveColourButton using) =
    rescaleRender (1/2) $ sequence_ [
        displaceRender (corner h) $ renderGlyph
            (TileGlyph (BlockTile [])
                (dim $ colourWheel (if using then h`div`2 else 1)))
        | h <- [0,2,4] ]

renderGlyph (ShowBlocksButton showing) = do
    renderGlyph (TileGlyph (BlockTile []) (dim red))
    when (showing == ShowBlocksAll) $
	renderGlyph (BlockedPush hu (bright orange))
    when (showing /= ShowBlocksNone) $
	renderGlyph (BlockedPush hw (bright purple))

renderGlyph (ShowButtonTextButton showing) = do
    rescaleRender (1/2) $ displaceRender (edge (neg hu)) $
        renderGlyph (ButtonGlyph (dim yellow))
    when showing $
	sequence_ [ pixelR (FVec (1/3 + i/4) (-1/4)) (bright white) | i <- [-1..1] ]

renderGlyph (UseSoundsButton use) = do
    sequence [ arcR (FVec (-2/3) 0) r (-20) 20
            (if use then bright green else dim red)
        | r <- [1/3,2/3,1] ]
    when (not use) $
        aaLineR (innerCorner hw) (innerCorner $ neg hw) $ dim red

renderGlyph (WhsButtonsButton Nothing) = rescaleRender (1/3) $ do
    renderGlyph (ButtonGlyph (dim red))
    sequence_ [ displaceRender ((3/2) **^ edge dir) $
            renderGlyph (ButtonGlyph (dim purple))
        | dir <- hexDirs ]
renderGlyph (WhsButtonsButton (Just whs)) = rescaleRender (1/2) $ do
    when (whs /= WHSHook) $
	displaceRender (corner 0) $ renderGlyph (TileGlyph (WrenchTile zero) col)
    when (whs /= WHSWrench) $ do
	displaceRender (corner 4) $ renderGlyph (TileGlyph HookTile col)
	displaceRender (corner 2) $ renderGlyph (TileGlyph (ArmTile hv False) col)
    where
	col = dim white

renderGlyph (FullscreenButton fs) = do
    thickPolygonR corners 1 $ activeCol (not fs)
    thickPolygonR corners' 1 $ activeCol fs
    where
	activeCol True = opaquify $ dim green
	activeCol False = opaquify $ dim red
	corners = [ (2/3) **^ (if dir `elem` [hu,neg hu] then edge else innerCorner) dir
	    | dir <- hexDirs ]
	corners' = map (((2/3)**^) . corner) [0..5]

renderGlyph (UnfreshGlyph) = do
    let col = bright red
    renderGlyph (HollowInnerGlyph col)
    sequence_ [pixelR (FVec (i/4) 0) col
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

-- |SVec: screen vectors, in pixels
data SVec = SVec { cx, cy :: Int }
    deriving (Eq, Ord, Show)
instance Monoid SVec where
    mempty = SVec 0 0
    mappend (SVec x y) (SVec x' y') = SVec (x+x') (y+y')
instance Grp SVec where
    neg (SVec x y) = SVec (-x) (-y)
type CCoord = PHS SVec

-- |FVec: floating point screen vectors, multiplied by size to get SVecs.
data FVec = FVec { rcx, rcy :: Float }
    deriving (Eq, Ord, Show)
instance Monoid FVec where
    mempty = FVec 0 0
    mappend (FVec x y) (FVec x' y') = FVec (x+x') (y+y')
instance Grp FVec where
    neg (FVec x y) = FVec (-x) (-y)

-- The following leads to overlapping instances (not sure why):
--instance MultAction Float FVec where
--    r *^ FVec x y = FVec (r*x) (r*y)
-- So instead, we define a new operator:
(**^) :: Float -> FVec -> FVec
r **^ FVec x y = FVec (r*x) (r*y)
    

hexVec2SVec :: Int -> HexVec -> SVec
hexVec2SVec size (HexVec x y z) =
    SVec ((x-z) * size) (-y * 3 * ysize size)

hexVec2FVec :: HexVec -> FVec
hexVec2FVec (HexVec x y z) =
    FVec (fi $ x-z) (-(fi y) * 3 * ylen)

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

displaceRender :: FVec -> RenderM a -> RenderM a
displaceRender v m = do
    size <- asks renderSize
    let FVec x y = fi size **^ v
    let sv = SVec (round x) (round y)
    displaceRenderSVec sv m

displaceRenderSVec :: SVec -> RenderM a -> RenderM a
displaceRenderSVec sv =
    local $ \rc -> rc { renderSCentre = renderSCentre rc +^ sv }

recentreAt :: HexVec -> RenderM a -> RenderM a
recentreAt v m = do
    size <- asks renderSize
    displaceRenderSVec (hexVec2SVec size v) m

rescaleRender :: RealFrac n => n -> RenderM a -> RenderM a
rescaleRender r = local resize
    where resize rc = rc { renderSize = round $ r * (fi $ renderSize rc) }

withFont :: Maybe TTF.Font -> RenderM a -> RenderM a
withFont font = local refont
    where refont rc = rc { renderFont = font }

renderPos :: Integral i => FVec -> RenderM (i,i)
renderPos v = do
    size <- asks renderSize
    let FVec dx dy = fi size **^ v
    SVec x y <- asks renderSCentre
    return $ (fi x + round dx, fi y + round dy)
renderLen :: Integral i => Float -> RenderM i
renderLen l = do
    size <- asks renderSize
    return $ round $ l * fi size

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
    size <- asks renderSize
    displaceRenderSVec (hexVec2SVec size v) $ renderGlyphCaching gl

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
    (surf, scrCentre, size) <- lift $ asks $ liftM3 (,,) renderSurf renderSCentre renderSize
    let SVec x y = scrCentre +^ (hexVec2SVec size v)
	    +^ neg (SVec 0 ((surfaceGetHeight fsurf-1)`div`2) +^
		if centred
		    then SVec ((surfaceGetWidth fsurf)`div`2) 0
		    else SVec 0 0)
    void $ liftIO $ blitSurface fsurf Nothing surf (Just $ Rect x y 0 0)

renderStrColAbove = renderStrColVShifted True
renderStrColBelow = renderStrColVShifted False
renderStrColVShifted :: Bool -> Pixel -> String -> HexVec -> RenderM ()
renderStrColVShifted up c str v =
    displaceRender (FVec 1 0) $ renderStrColAt c str $ v +^ (if up then hv else hw)

blankRow v = do
    (surf, scrCentre, size) <- asks $ liftM3 (,,) renderSurf renderSCentre renderSize
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
