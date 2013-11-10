module AsciiLock (lockToAscii, lockOfAscii, stateToAscii
    , readAsciiLockFile, writeAsciiLockFile) where

import Control.Applicative hiding ((<*>))
import Control.Exception
import System.IO
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector
import Data.Vector (Vector, (!), (//))
import Data.Maybe

import Lock
import GameStateTypes
import GameState
import CursesRender
import BoardColouring
import Frame
import Hex
import Util

type AsciiLock = [String]

lockToAscii :: Lock -> AsciiLock
lockToAscii (_,st) = stateToAscii st

stateToAscii :: GameState -> AsciiLock
stateToAscii st =
    let colouring = boardColouring st (ppidxs st) Map.empty
    in boardToAscii colouring . stateBoard $ st

lockOfAscii :: AsciiLock -> Lock
lockOfAscii lines = 
    let board = asciiToBoard lines
	size = maximum $ map (hx . (<->origin))$ Map.keys board
	frame = BasicFrame size
	st = asciiBoardState frame board
    in (frame, st)

boardToAscii :: PieceColouring -> GameBoard -> AsciiLock
boardToAscii colouring board =
    let asciiBoard :: Map CVec Char
	asciiBoard = Map.mapKeys (hexVec2CVec . (<->origin))
	    $ fmap (monochromeOTileChar colouring) board
	miny = (minimum $ map cy $ Map.keys asciiBoard)
	minx = (minimum $ map cx $ Map.keys asciiBoard)
	maxy = (maximum $ map cy $ Map.keys asciiBoard)
	maxx = (maximum $ map cx $ Map.keys asciiBoard)
	asciiBoard' = Map.mapKeys (<->CVec miny minx) asciiBoard
    in [ [ Map.findWithDefault ' ' (CVec y x) asciiBoard' 
	    | x <- [0..(maxx-minx)] ]
	| y <- [0..(maxy-miny)] ]

asciiToBoard :: AsciiLock -> GameBoard
asciiToBoard lines = 
    let asciiBoard :: Map CVec Char
	asciiBoard = Map.fromList [(CVec y x,ch)
	    | (line,y) <- zip lines [0..]
	    , (ch,x) <- zip line [0..]
	    , ch `notElem` "\t\r\n "]
	miny = minimum $ map cy $ Map.keys asciiBoard
	maxy = maximum $ map cy $ Map.keys asciiBoard
	midy = (maxy-miny)`div`2
	midline = filter ((==midy).cy) $ Map.keys asciiBoard
	minx = minimum $ map cx $ midline
	maxx = maximum $ map cx $ midline
	centre = CVec midy ((maxx-minx)`div`2)
    in Map.mapKeys ((<+>origin) . cVec2HexVec . (<->centre))
	$ fmap monoToOTile asciiBoard

asciiBoardState frame board =
    let addPreBase st = foldr addpp st (replicate 6 $ PlacedPiece origin $ Block [])
	addBase st = foldr addBaseOT st $ Map.toList $
	    Map.filter (isBaseTile.snd) board
	isBaseTile (BlockTile _) = True
	isBaseTile (PivotTile _) = True
	isBaseTile HookTile = True
	isBaseTile (WrenchTile _) = True
	isBaseTile (BallTile) = True
	isBaseTile _ = False
	addBaseOT :: (HexPos,(PieceIdx,Tile)) -> GameState -> GameState
	addBaseOT (pos,(o,BlockTile [])) = addBlockPos (o+1) pos
	addBaseOT (pos,(-1,t)) = addpp $ PlacedPiece pos $ basePieceOfTile t
	basePieceOfTile (PivotTile _) = Pivot []
	basePieceOfTile HookTile = Hook hu NullHF
	basePieceOfTile (WrenchTile _) = Wrench zero
	basePieceOfTile BallTile = Ball
	componentifyNew st = foldr ((fst.).componentify) st $ filter (/=0) $ ppidxs st
	setFrame st = fromMaybe st $ do
	    (idx,pp) <- listToMaybe [ (idx,pp)
		    | (idx,pp) <- enumVec $ placedPieces st
		    , isBlock . placedPiece $ pp
		    , let fp = plPieceFootprint pp
		    , not $ null fp
		    , all (not.inBounds frame) fp
		    ]
	    return $ delPiece idx $ setpp 0 pp st
	baseSt = setFrame . componentifyNew . addBase . addPreBase $ GameState Vector.empty []

	baseBoard = stateBoard baseSt
	addAppendages st = foldr addAppendageOT st $ Map.toList $
	    Map.filter (not.isBaseTile.snd) board
	addAppendageOT (pos,(-1,ArmTile dir _)) = 
	    let rpos = (neg dir<+>pos)
	    in case Map.lookup rpos baseBoard of
		Just (idx,PivotTile _) -> addPivotArm idx pos
		Just (idx,HookTile) -> setpp idx (PlacedPiece rpos (Hook dir NullHF))
	addAppendageOT (pos,(-1,SpringTile extn dir)) =
	    let rpos = (neg dir<+>pos)
	    in case Map.lookup rpos baseBoard of
		Just (ridx,BlockTile _) ->
		    let Just (_,epos) = castRay pos dir baseBoard
			twiceNatLen = sum [ extnValue extn
			    | i <- [1..hexLen (epos<->rpos)-1]
			    , let pos' = i<*>dir<+>rpos
			    , let Just (_,SpringTile extn _) = Map.lookup pos' board ]
			extnValue Compressed = 4
			extnValue Relaxed = 2
			extnValue Stretched = 1
			Just root = posLocus baseSt rpos
			Just end = posLocus baseSt epos
		    in addConn $ Connection root end (Spring dir $ twiceNatLen`div`2)
		_ -> id
	addAppendageOT _ = id
    in addAppendages $ baseSt

monochromeOTileChar :: PieceColouring -> OwnedTile -> Char
monochromeOTileChar colouring (idx,BlockTile _) =
    case Map.lookup idx colouring of
	Just 1 -> '%'
	Just 2 -> '"'
	Just 3 -> '&'
	Just 4 -> '~'
	_ -> '#'
monochromeOTileChar _ (_,t) = monochromeTileChar t
monochromeTileChar (PivotTile _) = 'o'
monochromeTileChar (ArmTile dir _) 
    | dir == hu = '-'
    | dir == hv = '\\'
    | dir == hw = '/'
    | dir == neg hu = '.'
    | dir == neg hv = ','
    | dir == neg hw = '\''
monochromeTileChar HookTile = '@'
monochromeTileChar (WrenchTile _) = '*'
monochromeTileChar BallTile = 'O'
monochromeTileChar (SpringTile extn dir)
    | dir == hu = case extn of
	Stretched                  -> 's'
	Relaxed                    -> 'S'
	Compressed                 -> '$'
    | dir == hv = case extn of
	Stretched                  -> 'z'
	Relaxed                    -> 'Z'
	Compressed                 -> '5'
    | dir == hw = case extn of
	Stretched                  -> '('
	Relaxed                    -> '['
	Compressed                 -> '{'
    | dir == neg hu = case extn of
	Stretched                  -> 'c'
	Relaxed                    -> 'C'
	Compressed                 -> 'D'
    | dir == neg hv = case extn of
	Stretched                  -> ')'
	Relaxed                    -> ']'
	Compressed                 -> '}'
    | dir == neg hw = case extn of
	Stretched                  -> '&'
	Relaxed                    -> '7'
	Compressed                 -> '9'
monoToOTile :: Char -> OwnedTile
monoToOTile '#' = (1,BlockTile [])
monoToOTile '%' = (2,BlockTile [])
monoToOTile '"' = (3,BlockTile [])
monoToOTile '&' = (4,BlockTile [])
monoToOTile '~' = (5,BlockTile [])
monoToOTile ch = (-1,monoToTile ch)
monoToTile 'o' = PivotTile zero
monoToTile '-' = ArmTile hu False
monoToTile '\\' = ArmTile hv False
monoToTile '/' = ArmTile hw False
monoToTile '.' = ArmTile (neg hu) False
monoToTile ',' = ArmTile (neg hv) False
monoToTile '\'' = ArmTile (neg hw) False
monoToTile '@' = HookTile
monoToTile '*' = WrenchTile zero
monoToTile 'O' = BallTile
monoToTile 's' = SpringTile Stretched hu
monoToTile 'S' = SpringTile Relaxed hu
monoToTile '$' = SpringTile Compressed hu
monoToTile 'z' = SpringTile Stretched hv
monoToTile 'Z' = SpringTile Relaxed hv
monoToTile '5' = SpringTile Compressed hv
monoToTile '(' = SpringTile Stretched hw
monoToTile '[' = SpringTile Relaxed hw
monoToTile '{' = SpringTile Compressed hw
monoToTile 'c' = SpringTile Stretched (neg hu)
monoToTile 'C' = SpringTile Relaxed (neg hu)
monoToTile 'D' = SpringTile Compressed (neg hu)
monoToTile ')' = SpringTile Stretched (neg hv)
monoToTile ']' = SpringTile Relaxed (neg hv)
monoToTile '}' = SpringTile Compressed (neg hv)
monoToTile '&' = SpringTile Stretched (neg hw)
monoToTile '7' = SpringTile Relaxed (neg hw)
monoToTile '9' = SpringTile Compressed (neg hw)


catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

readAsciiLockFile :: FilePath -> IO (Maybe Lock)
readAsciiLockFile path =
    (Just . lockOfAscii <$> (openFile path ReadMode >>= (lines <$>) . hGetContents))
	`catchAny` (const $ return Nothing)

writeAsciiLockFile :: FilePath -> Lock -> IO ()
writeAsciiLockFile path lock = do
    h <- openFile path WriteMode
    flip finally (hClose h) $ mapM_ (hPutStrLn h) $ lockToAscii lock

