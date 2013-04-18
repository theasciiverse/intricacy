module Main where

import qualified Data.Vector as Vector
import Control.Monad.State

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

frame n = PlacedPiece orig $ Block $ concat [
	    map (rotate r) [ (s<*>hu) <+> (n<*>hw) | s <- [0..n-1] ] | r <- [0..5] ]
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


data PlayState = PlayState {gameState::GameState, wrenchSelected::Bool }
type DisplayM = StateT (DisplayState, PlayState) IO

--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test initState = do
    ds <- initDisplay
    runStateT playState (ds, PlayState initState False)
    endDisplay
    where
	playState = do 
	    (ds@(DisplayState cpairs centre), ps@(PlayState st wsel)) <- get
	    liftIO $ drawState ds st
	    k <- liftIO $ getKey
	    if k == 'q'
	       then return ()
	       else let (wsel', pm) = moveOfKey wsel k
			st' = case pm of
			    Nothing -> st
			    Just pm' -> physicsTick pm' st
		    in put (ds, PlayState st' wsel') >> playState
	moveOfKey wsel k =
	    let push dir = if wsel then Just $ nullMove { wrenchPush = dir }
				   else Just $ nullMove { hookPush = dir }
		torque dir = if wsel then Nothing
				  else Just $ nullMove { hookTorque = dir }
	    in case k of
		    '*' -> (True, Nothing)
		    'o' -> (False, Nothing)
		    '@' -> (False, Nothing)
		    '\t' -> (not wsel, Nothing)
		    's' -> (wsel, push hu)
		    'h' -> (wsel, push $ neg hu)
		    'b' -> (wsel, push hw)
		    'c' -> (wsel, push $ neg hw)
		    'g' -> (wsel, push hv)
		    'm' -> (wsel, push $ neg hv)
		    'r' -> (wsel, torque 1)
		    'l' -> (wsel, torque $ -1)
		    ' ' -> (wsel, Just nullMove)
		    '.' -> (wsel, Just nullMove)
		    _ -> (wsel, Nothing)


main = test cogState
