module Main where

import qualified Data.Vector as Vector
import Control.Monad.State
import Data.List ((\\))
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Data.Maybe

import Hex
import CursesDisplay
import GameState
import Physics

ballState = GameState (Vector.fromList [
		PlacedPiece origin (Block [ zero, hu ])
		, PlacedPiece (((8::Int) <*> hv) <+> hu <+> origin) (Block [zero, neg hu])
		, PlacedPiece (((6::Int) <*> hv) <+> origin) (Ball zero)
		, PlacedPiece (((2::Int) <*> hv) <+> origin) (Ball zero)
	    ]) [
		Connection (0, hu) (1,zero) (Spring hv 4)
	    ] 
wrenchState = GameState (Vector.fromList [
		PlacedPiece origin (Block [ zero, hu ])
		, PlacedPiece (((5::Int) <*> hv) <+> hu <+> origin) (Block [zero, neg hu])
		, PlacedPiece (((1::Int) <*> hv) <+> origin) (Wrench hv)
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
    PlacedPiece origin $ Block $ [bolthole side <+> (side`div`4+1)<*>hu]
	++ (map (entrance side <+>) [neg hu <+> hv, 2 <*> neg hu, neg hu <+> hw,
		2 <*> hw, neg hv <+> hw])
	++ (concat [
		map (rotate r) [ (n<*>hu) <+> (side<*>hw) | n <- [0..side-1] ] | r <- [0..5] ] \\ 
	    [bolthole side, entrance side])
bolt :: Int -> PlacedPiece
bolt side = PlacedPiece (bolthole side <+> origin) $ Block $
    [ n<*>hu | n <- [0..side`div`4] ] -- ++ [(-2)<*>hu<+>neg hv]
boltbase :: Int -> PlacedPiece
boltbase side = PlacedPiece ((3*(side`div`4)+1) <*> neg hu <+> bolthole side <+> origin) $ Block [ zero ]
boltspring :: Int -> Connection
boltspring side = Connection (1,zero) (2,(-(side`div`4)) <*>hu) $ Spring hu $ 2*(side`div`4)


cogState = GameState (Vector.fromList [
		frame 24
		, PlacedPiece origin (Block [ zero ])
		, PlacedPiece (10 <*> hw <+> origin) (Block $ concat $
		    [ map (((2*n) <*> hw) <+>) [zero, hu, hw, hw<+>hw] | n <- [0..5] ])
		--, PlacedPiece (hu <+> hu <+> (14 <*> hw) <+> origin) (Pivot [ hu,hv,hw ] )
		, PlacedPiece ((6<*>hu) <+> (20<*>hw) <+> origin) (Ball $ zero)
		, PlacedPiece ((7<*>hu) <+> (17<*>hw) <+> origin) (Ball $ zero)
		, PlacedPiece ((3<*>hu) <+> (10<*>hw) <+> origin) (Wrench $ hw)
		, PlacedPiece ((3<*>hu) <+> origin) (Hook hv Unstiff)
		, PlacedPiece (7<*>hu <+> origin) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (10<*>hu <+> origin) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (12<*>hu <+> origin) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (3<*>hw <+> 10<*>hu <+> origin) (Pivot [ hu,hv,hw ] )
		, PlacedPiece (3<*>hw <+> neg hv <+> 11<*>hu <+> origin) (Pivot $ map neg [ hu,hv,hw ] )
	    ]) [
		Connection (1, zero) (2,zero) (Spring hw 5)
	    ] 

baseState side =
    GameState (Vector.fromList [
	frame side
	--, boltbase side
	, bolt side
	, PlacedPiece (entrance side <+> neg hu <+> origin) $ Wrench zero
	, PlacedPiece (entrance side <+> hw <+> origin) $ Hook (neg hw) Unstiff
    ]) [
	-- boltspring side
	]

data Command = CmdDir HexDir | CmdRotate TorqueDir | CmdWait
	| CmdToggle
	| CmdTile Tile | CmdMerge
	| CmdSelect
	| CmdEdit | CmdPlay | CmdTest
	| CmdQuit | CmdWriteState
    deriving (Eq, Ord, Show)
	
type KeyBindings = Map Char Command
dvorakBindings :: KeyBindings
dvorakBindings = Map.fromList [
	('o', CmdTile $ PivotTile zero)
	, ('O', CmdTile $ BallTile False)
	, ('-', CmdTile $ ArmTile hu False)
	, ('\\', CmdTile $ ArmTile hv False)
	, ('/', CmdTile $ ArmTile hw False)
	, ('S', CmdTile $ SpringTile Relaxed)
	, ('s', CmdTile $ SpringTile Stretched)
	, ('$', CmdTile $ SpringTile Compressed)
	, ('#', CmdTile BlockTile)
	, ('w', CmdTile BlockTile)
	, ('*', CmdTile $ WrenchTile False)
	, ('W', CmdTile $ WrenchTile False)
	, ('@', CmdTile HookTile)
	, ('\t', CmdToggle)
	, ('s', CmdDir hu)
	, ('h', CmdDir $ neg hu)
	, ('b', CmdDir hw)
	, ('c', CmdDir $ neg hw)
	, ('g', CmdDir hv)
	, ('m', CmdDir $ neg hv)
	, ('r', CmdRotate 1)
	, ('l', CmdRotate $ -1)
	, (' ', CmdSelect)
	, ('.', CmdWait)
	, ('M', CmdMerge)
	, ('q', CmdQuit)
	, ('e', CmdEdit)
	, ('p', CmdPlay)
	, ('T', CmdTest)
	, ('W', CmdWriteState)
	, ('\r', CmdSelect)
	, ('\n', CmdSelect)
	]


--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test :: KeyBindings -> Int -> GameState -> DisplayM ()
test bindings lockSize initState = do
    initDisplay
    EditState st _ _ <- execStateT editState $ EditState initState Nothing (PHS zero)
    endDisplay
    where
	playState :: StateT PlayState DisplayM ()
	playState = do 
	    ps@(PlayState st wsel) <- get
	    lift $ drawPlayState ps
	    k <- lift getKey
	    case Map.lookup k bindings of
		Nothing -> playState
		Just CmdQuit -> return ()
		Just CmdEdit -> do lift $ runStateT editState $ EditState st Nothing (PHS zero)
				   return ()
		Just cmd -> 
		    let push dir = if wsel then Just $ nullMove { wrenchPush = dir }
			    else Just $ nullMove { hookPush = dir }
			torque dir = if wsel then Nothing
			    else Just $ nullMove { hookTorque = dir }
			(wsel', pm) =
			    case cmd of
				CmdTile (WrenchTile _) -> (True, Nothing)
				CmdTile HookTile -> (False, Nothing)
				CmdToggle -> (not wsel, Nothing)
				CmdDir dir -> (wsel, push dir)
				CmdRotate dir -> (wsel, torque dir)
				CmdWait -> (wsel, Just nullMove)
				CmdSelect -> (wsel, Just nullMove)
				_ -> (wsel, Nothing)
		    in do
			st' <- case pm of
			     Nothing -> return st
			     Just pm' -> lift $ doPhysicsTick pm' st
			put (PlayState st' wsel') >> playState

	editState :: StateT EditState DisplayM ()
	editState = do 
	    es@(EditState st selPiece selPos) <- get
	    lift $ drawEditState es
	    k <- lift getKey
	    case Map.lookup k bindings of
		Nothing -> editState
		Just CmdQuit -> return ()
		Just CmdPlay -> void $ lift $ runStateT playState $ PlayState st False
		Just CmdTest -> do
		    let st' = canonify st
		    put es {esGameState = canonify st'} 
		    void $ lift $ runStateT playState $ PlayState st' False
		    editState
			    
		Just cmd ->
		    let doForce force = 
			    let (st',alerts) = runWriter $ resolveForces (Vector.singleton force) True st
			    in lift (drawAlerts alerts) >> put es {esGameState = st'} >> editState
		    in case cmd of
		        CmdSelect ->
			    let selPiece' =
				    if isJust selPiece
					then Nothing
					else liftM fst $ Map.lookup selPos $ stateBoard st
			    in put es {selectedPiece = selPiece'} >> editState
			CmdDir dir -> 
			    case selPiece of
				Nothing -> put es {selectedPos = checkBounds selPos $ dir <+> selPos} >> editState
				Just p -> doForce $ Push p dir
			CmdRotate dir ->
			    case selPiece of
				Nothing -> editState
				Just p -> doForce $ Torque p dir
			CmdTile tile ->
			    let st' = modTile (Just tile) selPos st
			    in put es {esGameState = st'} >> editState
			CmdMerge -> lift getKey >>= \k ->
			    case Map.lookup k bindings of
				Just (CmdDir mergeDir) ->
				    let st' = mergeTiles selPos mergeDir True st
				    in put es {esGameState = st'} >> editState
				_ -> editState
			CmdWait ->
			    case selPiece of
				Nothing -> put es {esGameState = modTile Nothing selPos st} >> editState
				Just p -> do st' <- lift $ doPhysicsTick nullMove st
					     put es {esGameState = st'} >> editState
			CmdWriteState -> (liftIO $ writeFile "state" $ show st) >> editState
			_ -> editState
	checkBounds :: HexPos -> HexPos -> HexPos
	checkBounds def pos = if hexLen (pos <-> origin) < lockSize then pos else def
	canonify st = addTools $ stabilise $ delTools st
	delTools st = case listToMaybe [ idx | (idx,pp) <- enumVec $ placedPieces st
			      , isTool $ placedPiece pp ] of
			Nothing -> st
			Just idx -> delTools $ delPiece idx st
	addTools st = foldr addpp st
	    [ PlacedPiece (entrance lockSize <+> neg hu <+> origin) $ Wrench zero,
	    PlacedPiece (entrance lockSize <+> hw <+> origin) $ Hook (neg hw) Unstiff ]
	stabilise st =
	    let (st',_) = runWriter $ physicsTick nullMove st
	    in if st == st' then st else stabilise st'
    
	doPhysicsTick :: PlayerMove -> GameState -> DisplayM GameState
	doPhysicsTick pm st =
	    let (st',alerts) = runWriter $ physicsTick pm st in
	    drawAlerts alerts >> return st'


main =
    let size = 8
    in test dvorakBindings size (baseState size) `runStateT` nullDisplayState
