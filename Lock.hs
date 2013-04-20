module Main where

import qualified Data.Vector as Vector
import Control.Monad.State
import Data.List ((\\))
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer

import Hex
import CursesDisplay
import GameState
import Physics

orig = PHS zero
ballState = GameState (Vector.fromList [
		PlacedPiece orig (Block [ zero, hu ])
		, PlacedPiece (((8::Int) <*> hv) <+> hu <+> orig) (Block [zero, neg hu])
		, PlacedPiece (((6::Int) <*> hv) <+> orig) (Ball zero)
		, PlacedPiece (((2::Int) <*> hv) <+> orig) (Ball zero)
	    ]) [
		Connection (0, hu) (1,zero) (Spring hv 4)
	    ] 
wrenchState = GameState (Vector.fromList [
		PlacedPiece orig (Block [ zero, hu ])
		, PlacedPiece (((5::Int) <*> hv) <+> hu <+> orig) (Block [zero, neg hu])
		, PlacedPiece (((1::Int) <*> hv) <+> orig) (Wrench hv)
	    ]) [
		Connection (0, hu) (1,zero) (Spring hv 2)
	    ] 

bolthole :: Int -> HexVec
bolthole side = side<*>hu <+> (side`div`2)<*>hv
entrance :: Int -> HexVec
-- entrance side = (side-1)<*> neg hu <+> (side-1)`div`2<*>neg hv
entrance side = neg $ bolthole side
frame :: Int -> PlacedPiece
frame side =
    PlacedPiece orig $ Block $ [bolthole side <+> (side`div`4+1)<*>hu]
	++ (map (entrance side <+>) [neg hu <+> hv, 2 <*> neg hu, neg hu <+> hw,
		2 <*> hw, neg hv <+> hw])
	++ (concat [
		map (rotate r) [ (n<*>hu) <+> (side<*>hw) | n <- [0..side-1] ] | r <- [0..5] ] \\ 
	    [bolthole side, entrance side])
bolt :: Int -> PlacedPiece
bolt side = PlacedPiece (bolthole side <+> orig) $ Block $
    [ n<*>hu | n <- [-side`div`4..side`div`4] ] ++ [(-2)<*>hu<+>neg hv]
boltbase :: Int -> PlacedPiece
boltbase side = PlacedPiece ((3*(side`div`4)+1) <*> neg hu <+> bolthole side <+> orig) $ Block [ zero ]
boltspring :: Int -> Connection
boltspring side = Connection (1,zero) (2,(-(side`div`4)) <*>hu) $ Spring hu $ 2*(side`div`4)


cogState = GameState (Vector.fromList [
		frame 24
		, PlacedPiece orig (Block [ zero ])
		, PlacedPiece (10 <*> hw <+> orig) (Block $ concat $
		    [ map (((2*n) <*> hw) <+>) [zero, hu, hw, hw<+>hw] | n <- [0..5] ])
		--, PlacedPiece (hu <+> hu <+> (14 <*> hw) <+> orig) (Pivot [ hu,hv,hw ] )
		, PlacedPiece ((6<*>hu) <+> (20<*>hw) <+> orig) (Ball $ zero)
		, PlacedPiece ((7<*>hu) <+> (17<*>hw) <+> orig) (Ball $ zero)
		, PlacedPiece ((3<*>hu) <+> (10<*>hw) <+> orig) (Wrench $ hw)
		, PlacedPiece ((3<*>hu) <+> orig) (Hook hv False)
		, PlacedPiece (7<*>hu <+> orig) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (10<*>hu <+> orig) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (12<*>hu <+> orig) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (3<*>hw <+> 10<*>hu <+> orig) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (3<*>hw <+> neg hv <+> 11<*>hu <+> orig) (Pivot $ map neg [ hu,hv,hw ] )
	    ]) [
		Connection (1, zero) (2,zero) (Spring hw 5)
	    ] 

baseState = let side = 11 in
    GameState (Vector.fromList [
	frame side
	, boltbase side
	, bolt side
	, PlacedPiece (entrance side <+> neg hu <+> orig) $ Wrench zero
	, PlacedPiece (entrance side <+> hw <+> orig) $ Hook (neg hw) False
    ]) [
	boltspring side
	]

data Command = CmdDir HexDir | CmdRotate TorqueDir | CmdWait
	| CmdFlipFocus | CmdWrench | CmdHook
	| CmdQuit
	
type KeyBindings = Map Char Command
dvorakBindings :: KeyBindings
dvorakBindings = Map.fromList [
	('*', CmdWrench)
	, ('o', CmdHook)
	, ('@', CmdHook)
	, ('\t', CmdFlipFocus)
	, ('s', CmdDir hu)
	, ('h', CmdDir $ neg hu)
	, ('b', CmdDir hw)
	, ('c', CmdDir $ neg hw)
	, ('g', CmdDir hv)
	, ('m', CmdDir $ neg hv)
	, ('r', CmdRotate 1)
	, ('l', CmdRotate $ -1)
	, (' ', CmdWait)
	, ('.', CmdWait)
	, ('q', CmdQuit)
	]


--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test bindings initState = do
    ds <- initDisplay
    runStateT playState (ds, PlayState initState False)
    endDisplay
    where
	playState :: StateT (DisplayState, PlayState) IO ()
	playState = do 
	    (ds@(DisplayState cpairs centre), ps@(PlayState st wsel)) <- get
	    liftIO $ drawPlayState ds ps
	    k <- liftIO $ getKey
	    case Map.lookup k bindings of
		Nothing -> playState
		Just CmdQuit -> return ()
		Just cmd -> 
		    let push dir = if wsel then Just $ nullMove { wrenchPush = dir }
			    else Just $ nullMove { hookPush = dir }
			torque dir = if wsel then Nothing
			    else Just $ nullMove { hookTorque = dir }
			(wsel', pm) =
			    case cmd of
				CmdWrench -> (True, Nothing)
				CmdHook -> (False, Nothing)
				CmdFlipFocus -> (not wsel, Nothing)
				CmdDir dir -> (wsel, push dir)
				CmdRotate dir -> (wsel, torque dir)
				CmdWait -> (wsel, Just nullMove)
				_ -> (wsel, Nothing)
			(st', alerts) = case pm of
			     Nothing -> (st,[])
			     Just pm' -> runWriter $ physicsTick pm' st
		    in liftIO (drawAlerts ds alerts) >> put (ds, PlayState st' wsel') >> playState

	editState :: StateT (DisplayState, EditState) IO ()
	editState = do 
	    (ds@(DisplayState cpairs centre), es@(EditState st psel)) <- get
	    liftIO $ drawEditState ds es
	    k <- liftIO $ getKey
	    case Map.lookup k bindings of
		Nothing -> editState
		Just CmdQuit -> return ()
		Just cmd -> 
		   put (ds, EditState st psel) >> editState


main = test dvorakBindings baseState
