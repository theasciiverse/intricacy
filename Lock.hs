module Main where

import qualified Data.Vector as Vector
import Control.Monad.State
import Data.List ((\\))
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Data.Maybe

import Control.Exception
import System.IO.Error

import System.Environment
import System.Console.GetOpt

import Hex
import CursesDisplay
import GameState
import Physics

bolthole :: Int -> HexVec
bolthole side = side<*>hu <+> (side`div`2)<*>hv
entrance :: Int -> HexVec
-- entrance side = (side-1)<*> neg hu <+> (side-1)`div`2<*>neg hv
entrance side = neg $ bolthole side
frame :: Int -> PlacedPiece
frame side =
    PlacedPiece origin $ Block $ [bolthole side <+> (side`div`4+1)<*>hu <+> n<*>hv | n <- [0..side`div`4+1] ]
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
test :: KeyBindings -> Int -> Maybe String -> GameState -> DisplayM ()
test bindings lockSize fname initState = do
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
			CmdWriteState ->
			    let wfname = fromMaybe "unnamed.lock" fname 
			    in (liftIO $ writeFile wfname $ show (lockSize, canonify st)) >> editState
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


data Opt = LockSize Int
options = [
    Option ['s'] ["locksize"] (ReqArg (LockSize . read) "SIZE") "locksize"
    ]
parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
	(o,n,[]) -> return (o,n)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: lock [OPTION...] [file]"

readLockFile :: String -> IO (Maybe (Int, GameState))
readLockFile file = do
    lockstr <- liftIO $ catchJust (guard . isDoesNotExistError)
		  (readFile file)
		  (\_ -> return "")
    return $ length lockstr `seq` -- XXX: forces file to be read and closed
	(liftM fst $ listToMaybe $ reads lockstr)
    
main = do
    argv <- getArgs
    (opts,args) <- parseArgs argv
    let initSize = fromMaybe 8 $ listToMaybe [ size | LockSize size <- opts ]
    let fname = listToMaybe args
    lock <- case fname of
		Nothing -> return Nothing
		Just fname -> readLockFile fname
    let (size, initState) = fromMaybe (initSize, baseState initSize) $ lock
    test dvorakBindings size fname initState `runStateT` nullDisplayState
