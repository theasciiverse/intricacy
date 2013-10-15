module Frame where

import Data.List ((\\))
import qualified Data.Vector as Vector

import GameState
import Hex

data Frame = BasicFrame Int
    deriving (Eq, Ord, Show, Read)

bolthole size = size<*>hu <+> (size`div`2)<*>hv
entrance size = neg $ bolthole size

baseState :: Frame -> GameState
baseState fr@(BasicFrame size) =
    GameState 
	(Vector.fromList $ [ framePiece fr, bolt size ] ++ (initTools fr))
	[]
    where
	bolt :: Int -> PlacedPiece
	bolt size = PlacedPiece (bolthole size <+> origin) $ Block $
	    [ n<*>hu | n <- [-1..size`div`4] ] -- ++ [(-2)<*>hu<+>neg hv]

framePiece :: Frame -> PlacedPiece
framePiece (BasicFrame size) =
    PlacedPiece origin $ Block $
	map (bolthole size <+>) (
	    [ bw<*>hu <+> n<*>hv | n <- [0..bw] ]
	    ++ [ bw<*>hu <+> i<*>hw <+> n<*>hv | i <- [1..bw-1], n <- [0,bw+i] ])
	++ (map (entrance size <+>) [neg hu <+> hv, 2 <*> neg hu, neg hu <+> hw,
		2 <*> hw, neg hv <+> hw])
	++ (concat [
		map (rotate r) [ (n<*>hu) <+> (size<*>hw) | n <- [0..size-1] ] | r <- [0..5] ] \\ 
	    [bolthole size, entrance size])
    where bw = (size`div`4+1)

initTools :: Frame -> [PlacedPiece]
initTools (BasicFrame size) =
    [ PlacedPiece (entrance size <+> neg hu <+> origin) $ Wrench zero,
    PlacedPiece (entrance size <+> hw <+> origin) $ Hook (neg hw) NullHF ]

clearToolArea :: Frame -> GameState -> GameState
clearToolArea (BasicFrame size) st = foldr delPieceIn st [entrance size <+> v <+> origin
	    | v <- [ neg hu, hw, zero ] ]

frameSize :: Frame -> Int
frameSize (BasicFrame size) = size

boltArea :: Frame -> [HexPos]
boltArea (BasicFrame size) = map PHS
	[ bolthole size <+> bw<*>hu <+> i<*>hw <+> n<*>hv | i <- [1..bw-1], n <- [1..bw+i-1] ]
    where bw = (size`div`4+1)

inBounds :: Frame -> HexPos -> Bool
inBounds f pos = hexLen (pos <-> origin) < frameSize f
checkBounds :: Frame -> HexPos -> HexPos -> HexPos
checkBounds f def pos = if inBounds f pos then pos else def
truncateToBounds :: Frame -> HexPos -> HexPos
truncateToBounds f pos@(PHS v) =
    if inBounds f pos
	then pos
	else PHS $ scaleToLength ((frameSize f)-1) v
