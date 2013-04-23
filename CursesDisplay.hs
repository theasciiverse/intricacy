module CursesDisplay ( drawState, drawPlayState, drawEditState, drawAlerts
    , DisplayState(DisplayState), DisplayM, nullDisplayState, initDisplay, endDisplay, getKey ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State

import Hex
import GameState

type AttrChar = (Char, Curses.Attr)
type ColPair = Int
data Glyph = Glyph Char ColPair Curses.Attr
tileChar :: Tile -> AttrChar
a0 = Curses.attr0
bold = Curses.setBold a0 True
tileChar BlockTile = ('#',a0)
tileChar (PivotTile dir) | dir == zero = ('o',bold)
    | canonDir dir == hu = ('-',bold)
    | canonDir dir == hv = ('\\',bold)
    | canonDir dir == hw = ('/',bold)
tileChar (ArmTile dir principal) = let
    cdir = canonDir dir
    c | cdir == hu = '-'
      | cdir == hv = '\\'
      | cdir == hw = '/'
    a = if principal then bold else a0
    in (c,a)
tileChar HookTile = ('@',bold)
tileChar (WrenchTile moving) = ('*',if moving then bold else a0)
tileChar (BallTile moving) = ('O',if moving then bold else a0)
tileChar (SpringTile Relaxed) = ('S',a0)
tileChar (SpringTile Compressed) = ('$',bold)
tileChar (SpringTile Stretched) = ('s',bold)

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring reversed (owner,t) =
    let (ch,attr) = tileChar t
	pair = case Map.lookup owner colouring of
		    Nothing -> 0
		    Just n -> n+1
	reverse = owner `elem` reversed
    in Glyph ch pair (Curses.setReverse attr reverse)

data CVec = CVec { cy, cx :: Int }
    deriving (Eq, Ord, Show)
instance Monoid CVec where
    mempty = CVec 0 0
    mappend (CVec y x) (CVec y' x') = CVec (y+y') (x+x')
instance Grp CVec where
    neg (CVec y x) = CVec (-y) (-x) 
type CCoord = PHS CVec

hexVec2CVec :: HexVec -> CVec
hexVec2CVec (HexVec x y z) = CVec (-y) (x-z) 

addCh :: Char ->  IO ()
addCh c = Curses.wAddStr Curses.stdScr [c]
mvAddCh :: CVec -> Char ->  IO ()
mvAddCh (CVec y x) c = Curses.mvAddCh y x $ fromIntegral $ ord c
mvAddGlyph :: [Curses.Pair] -> CVec -> Glyph -> IO ()
mvAddGlyph cpairs v (Glyph ch col attr) =
    Curses.attrSet attr (cpairs!!col) >> mvAddCh v ch
move :: CVec -> IO ()
move (CVec y x) = Curses.move y x

data DisplayState = DisplayState { dispCPairs::[Curses.Pair], dispCentre::HexPos, dispLastCol::PieceColouring }
type DisplayM = StateT DisplayState IO
nullDisplayState = DisplayState [] (PHS zero) Map.empty


erase :: DisplayM ()
erase = liftIO Curses.erase
refresh :: DisplayM ()
refresh = liftIO Curses.refresh

drawAt :: Glyph -> HexPos -> DisplayM ()
drawAt gl pos = do
	scrCentre <- getScrCentre
	cpairs <- gets dispCPairs
	centre <- gets dispCentre
	let cpos = scrCentre <+> (hexVec2CVec $ pos <-> centre)
	liftIO $ mvAddGlyph cpairs cpos gl

drawCursorAt :: Maybe HexPos -> DisplayM ()
drawCursorAt Nothing =
    liftIO $ Curses.cursSet Curses.CursorInvisible >> return ()
drawCursorAt (Just pos) = do 
    scrCentre <- getScrCentre
    centre <- gets dispCentre
    liftIO $ Curses.cursSet Curses.CursorVisible
    liftIO $ move $ scrCentre <+> (hexVec2CVec $ pos <-> centre)
    refresh

getScrCentre :: DisplayM CVec
getScrCentre = do 
    (h,w) <- liftIO Curses.scrSize
    return $ CVec (h`div`2) (w`div`2)

-- TODO: monochrome mode (putting numbers in adjacent squares in place of
-- colours?)
drawState :: [PieceIdx] -> Bool -> GameState -> DisplayM ()
drawState reversed colourFixed st =
    let board = stateBoard st
    in do
	DisplayState cpairs centre lastCol <- get
	let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
	modify $ \ds -> ds { dispLastCol = colouring }
	erase
	sequence_ [ drawAt glyph pos | 
	    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring reversed) board
	    ]
	refresh

drawPlayState :: PlayState -> DisplayM ()
drawPlayState (PlayState st wsel) =
    do
	ds@(DisplayState _ centre _) <- get
	drawState [] False st
	drawCursorAt $ listToMaybe [ pos |
	    (_, PlacedPiece pos p) <- enumVec $ placedPieces st
	    , or [wsel && isWrench p, not wsel && isHook p] ]

drawEditState :: EditState -> DisplayM ()
drawEditState (EditState st selPiece selPos) =
    do
	ds@(DisplayState _ centre _) <- get
	drawState (maybeToList selPiece) True st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing

drawAlerts :: [Alert] -> DisplayM ()
drawAlerts = mapM_ drawAlert . take 1
    where
	drawAlert (AlertCollision pos) =
	    do  
		drawAt cGlyph pos
		refresh
		liftIO $ threadDelay $ 1*10^5
	cGlyph = Glyph '!' 0 a0

-- From Curses.CursesHelper:
-- | Converts a list of 'Curses.Color' pairs (foreground color and
--   background color) into the curses representation 'Curses.Pair'.
colorsToPairs :: [(Curses.Color, Curses.Color)] -> IO [Curses.Pair]
colorsToPairs cs =
    do p <- Curses.colorPairs
       let nColors = length cs
           blackWhite = p < nColors
       if blackWhite then do
	    print ("Terminal does not support enough colors. Number of " ++
                      " colors requested: " ++ show nColors ++
                      ". Number of colors supported: " ++ show p)
	    return $ take nColors (repeat (Curses.Pair 0))
          else mapM toPairs (zip [1..] cs)
     where toPairs (n, (fg, bg)) =
               let p = Curses.Pair n
               in do Curses.initPair p fg bg
                     return p

initDisplay :: DisplayM ()
initDisplay = 
    do liftIO $ CursesH.start 
       cpairs <- liftIO $ colorsToPairs [ (f, CursesH.black) | f <-
	    [ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue, CursesH.magenta] ] 
       put $ DisplayState cpairs (PHS zero) Map.empty

endDisplay :: DisplayM ()
endDisplay = liftIO CursesH.end

getKey :: DisplayM Char
getKey = do Curses.KeyChar ch <- liftIO $ CursesH.getKey Curses.refresh
	    return ch
