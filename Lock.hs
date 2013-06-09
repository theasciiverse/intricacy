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
-- import CursesUI
import SDLUI
import GameState
import Physics
import Command

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


--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test :: Int -> Maybe String -> GameState -> UIM ()
test lockSize fname initState = do
    initUI
    runStateT edit $ EditState [initState] Nothing (PHS zero) Nothing
    endUI
    where
	play :: StateT PlayState UIM ()
	play = do 
	    ps@(PlayState (st:sts) wsel newMode) <- get
	    let
		processPlayCommand CmdQuit =
		    put ps {psSwitchMode = Just GMQuit}
	        processPlayCommand CmdUndo =
		    when (not $ null sts) $ put ps {psGameStateStack = sts}
		processPlayCommand cmd = 
		    let push whs dir 
			    | whs == WHSWrench || (whs == WHSSelected && wsel) =
				Just $ nullMove { wrenchPush = dir }
			    | otherwise =
				Just $ nullMove { hookPush = dir }
			torque dir = -- if wsel then Nothing else
			    Just $ nullMove { hookTorque = dir }
			(wsel', pm) =
			    case cmd of
				CmdTile (WrenchTile _) -> (True, Nothing)
				CmdTile HookTile -> (False, Nothing)
				CmdTile (ArmTile _ _) -> (False, Nothing)
				CmdToggle -> (not wsel, Nothing)
				CmdDir whs dir -> (wsel, push whs dir)
				CmdRotate dir -> (wsel, torque dir)
				CmdWait -> (wsel, Just nullMove)
				CmdSelect -> (wsel, Just nullMove)
				_ -> (wsel, Nothing)
		    in do
			put $ ps {wrenchSelected = wsel'}
			case pm of
			     Nothing -> return ()
			     Just pm' -> (lift $ doPhysicsTick pm' st) >>= pushPState
		in case newMode of
		    Just GMQuit -> return ()
		    _ -> do
			lift $ drawPlayState ps
			cmds <- lift $ getInput $ GMPlay False
			sequence_ $ map processPlayCommand cmds
			play

	edit :: StateT EditState UIM ()
	edit = do 
	    es@(EditState (st:sts) selPiece selPos newMode) <- get
	    let
		doForce force = 
		    let (st',alerts) = runWriter $ resolveForces (Vector.singleton force) Vector.empty st
		    in lift (drawAlerts alerts) >> pushEState st'
		processEditCommand CmdQuit =
		    put es {esSwitchMode = Just GMQuit}
		processEditCommand CmdPlay =
		    put es {esSwitchMode = Just $ GMPlay False}
		processEditCommand CmdTest =
		    put es {esSwitchMode = Just $ GMPlay True}
		processEditCommand CmdUndo =
		    when (not $ null sts) $ put es {esGameStateStack = sts}
			    
		processEditCommand CmdSelect =
		    let selPiece' =
			    if isJust selPiece
				then Nothing
				else liftM fst $ Map.lookup selPos $ stateBoard st
		    in put es {selectedPiece = selPiece'}
		processEditCommand (CmdDir _ dir) =
		    case selPiece of
			Nothing -> put es {selectedPos = checkBounds selPos $ dir <+> selPos}
			Just p -> doForce $ Push p dir
		processEditCommand (CmdRotate dir) =
		    case selPiece of
			Nothing -> return ()
			Just p -> doForce $ Torque p dir
		processEditCommand (CmdTile tile) =
		    pushEState (modTile (Just tile) selPos st)
		processEditCommand CmdMerge = do
		    cmds <- lift $ getInput GMEdit
		    case cmds of
			[CmdDir _ mergeDir] ->
			    let st' = mergeTiles selPos mergeDir True st
			    in pushEState st'
			_ -> return ()
		processEditCommand CmdWait =
		    case selPiece of
			Nothing -> pushEState (modTile Nothing selPos st)
			Just p -> do st' <- lift $ doPhysicsTick nullMove st
				     pushEState st'
		processEditCommand CmdDelete =
		    case selPiece of
			Nothing -> return ()
			Just p -> do put es {selectedPiece = Nothing}
				     pushEState $ delPiece p st
		processEditCommand CmdWriteState =
		    let wfname = fromMaybe "unnamed.lock" fname 
		    in (liftIO $ writeFile wfname $ show (lockSize, canonify st))
		processEditCommand _ = return ()
		in case newMode of
		    Just GMQuit -> return ()
		    Just (GMPlay False) -> subPlay st
		    Just (GMPlay True) -> do
			let st' = canonify st
			pushEState st'
			subPlay st'
		    _ -> do
			lift $ drawEditState es
			cmds <- lift $ getInput GMEdit
			sequence_ $ map processEditCommand cmds
			edit
	    where
		subPlay st = do
		    modify $ \es -> es {esSwitchMode = Nothing}
		    void $ lift $ runStateT play $ PlayState [st] False Nothing
		    edit

	pushEState :: GameState -> StateT EditState UIM ()
	pushEState st = do
	    es@(EditState (st':sts) _ _ _) <- get
	    when (st' /= st) $ put $ es {esGameStateStack = st:st':sts}
	pushPState :: GameState -> StateT PlayState UIM ()
	pushPState st = do
	    ps@(PlayState (st':sts) _ _) <- get
	    when (st' /= st) $ put $ ps {psGameStateStack = st:st':sts}
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
    
	doPhysicsTick :: PlayerMove -> GameState -> UIM GameState
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
    test size fname initState `runStateT` nullUIState
