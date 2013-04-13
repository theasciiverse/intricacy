module Main where

import Hex
import qualified Data.Map as Map
import Data.Monoid

import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

data EntityType = Wall | Spring Bool | Block | Wrench HexDir | HookBase | Hook
    deriving (Eq, Ord, Show)
data Entity = Entity { tp :: EntityType, links :: [HexDir] }
    deriving (Eq, Ord, Show)

type GameSpace = PHS HexVec
type HexBoard = Map.Map GameSpace Entity

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
mvAddCh (CVec y x) c = Curses.move y x >> Curses.wAddStr Curses.stdScr [c]

drawBoard :: HexBoard -> GameSpace -> IO ()
drawBoard board centre =
    do	(h,w) <- Curses.scrSize
	let scrCentre = CVec (h`div`2) (w`div`2)
	sequence_ [ mvAddCh cc ch | 
		    (pos,ch) <- Map.toList $ Map.map entityChar board,
		    cc <- [ scrCentre <+> (hexVec2CVec (pos <-> centre)) ]
		]
	Curses.refresh

entityChar :: Entity -> Char
entityChar (Entity Hook [hu]) = '-'
entityChar (Entity Hook [hv]) = '/'
entityChar (Entity Hook [hw]) = '\\'
entityChar e = entityTpChar $ tp e
entityTpChar Wall = '#'
entityTpChar Block = '%'
entityTpChar (Spring True) = '$'
entityTpChar (Spring False) = 'S'
entityTpChar (Wrench _) = '*'
entityTpChar (HookBase) = '@'
entityTpChar _ = '?'

physicsTick :: HexBoard -> HexBoard


-- testBoard = Map.insert (PHS zero) (Entity (Spring True) zero) Map.empty
testBoard = Map.fromList [ (PHS ((-n) <*> hw), if n<2 then Entity (Spring False) []
					       else if n<6 then Entity Block []
					       else Entity (Wrench hw) [])
					       | n <- [0..6::Int] ]

test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end

main = test
