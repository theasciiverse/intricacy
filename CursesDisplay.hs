module CursesDisplay ( drawState, DisplayState(DisplayState), initDisplay, endDisplay, getKey ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)

import Hex
import GameState

type AttrChar = (Char, Curses.Attr)
type ColPair = Int
data Glyph = Glyph Char ColPair Curses.Attr
tileChar :: Tile -> AttrChar
a0 = Curses.attr0
bold = Curses.setBold a0 True
tileChar BlockTile = ('#',a0)
tileChar PivotTile = ('o',bold)
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

ownedTileGlyph :: PieceColouring -> OwnedTile -> Glyph
ownedTileGlyph colouring (owner,t) =
    let (ch,attr) = tileChar t
	pair = case Map.lookup owner colouring of
		    Nothing -> 0
		    Just n -> n+1
    in Glyph ch pair attr

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

data DisplayState = DisplayState { dispCPairs::[Curses.Pair], dispCentre::HexPos }

drawState :: DisplayState -> GameState -> IO ()
drawState (DisplayState cpairs centre) st =
    let board = stateBoard st
	colouring = boardColouring board coloured
	coloured = [ idx |
	    (idx,pp@(PlacedPiece pos p)) <- enumVec $ placedPieces st
	    , case p of (Ball _) -> False
			(Wrench _) -> False
			(Hook _ _) -> False
			(Pivot _) -> True
			(Block _) -> not $ null $ springsAtIdx st idx ]
    in do
	(h,w) <- Curses.scrSize
	let scrCentre = CVec (h`div`2) (w`div`2)
	Curses.erase
	sequence_ [ mvAddGlyph cpairs cc glyph | 
	    (pos,glyph) <- Map.toList $ Map.map (ownedTileGlyph colouring) board
	    , cc <- [ scrCentre <+> (hexVec2CVec (pos <-> centre)) ]
	    ]
	Curses.refresh

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

initDisplay = 
    do CursesH.start 
       cpairs <- colorsToPairs [ (f, CursesH.black) | f <-
	    [ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue] ] 
       return $ DisplayState cpairs (PHS zero)

endDisplay = CursesH.end

getKey = do Curses.KeyChar ch <- CursesH.getKey Curses.refresh
	    return ch
