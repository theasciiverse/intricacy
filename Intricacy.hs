module Main where

import Control.Monad.State
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
import UI
import qualified SDLUI
import qualified CursesUI
import GameState
import Physics
import Command
import Frame

type Lock = (Frame, GameState)

--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test :: UIMonad uiM => Lock -> Maybe String -> uiM Bool
test lock@(frame,initState) fname = do
    ok <- initUI
    if not ok
	then return False
	else do
	    runStateT edit $ EditState [initState] [] Nothing (PHS zero) (PHS zero) Nothing
	    endUI
	    return True
    where
	play :: UIMonad uiM => StateT PlayState uiM ()
	play = do
	    ps <- get
	    case psSwitchMode ps of
		Just GMQuit -> return ()
		_ -> do
		    lift $ drawPlayState ps
		    cmds <- lift $ getInput $ IMPlay
		    sequence_ $ map processPlayCommand cmds
		    play
	    where
		processPlayCommand CmdQuit =
		    modify $ \ps -> ps {psSwitchMode = Just GMQuit}
	        processPlayCommand CmdUndo = do
		    st:sts <- gets psGameStateStack
		    usts <- gets psUndoneStack
		    when (not $ null sts) $ modify $ \ps -> ps {psGameStateStack = sts, psLastAlerts = [], psUndoneStack = st:usts}
		processPlayCommand CmdRedo = do
		    usts <- gets psUndoneStack
		    case usts of 
			[] -> return ()
			ust:usts' -> do
			    pushPState ust
			    modify $ \ps -> ps {psUndoneStack = usts'}
		processPlayCommand cmd = do
		    wsel <- gets wrenchSelected
		    st:_ <- gets psGameStateStack
		    let push whs dir 
			    | whs == WHSWrench || (whs == WHSSelected && wsel) =
				Just $ WrenchPush dir
			    | otherwise = Just $ HookPush dir
			torque dir = Just $ HookTorque dir
			(wsel', pm) =
			    case cmd of
				CmdTile (WrenchTile _) -> (True, Nothing)
				CmdTile HookTile -> (False, Nothing)
				CmdTile (ArmTile _ _) -> (False, Nothing)
				CmdToggle -> (not wsel, Nothing)
				CmdDir whs dir -> (wsel, push whs dir)
				CmdRotate dir -> (wsel, torque dir)
				CmdWait -> (wsel, Just NullPM)
				CmdSelect -> (wsel, Just NullPM)
				_ -> (wsel, Nothing)
		    modify $ \ps -> ps {wrenchSelected = wsel'}
		    case pm of
			 Nothing -> return ()
			 Just pm' -> do
			    (st',alerts) <- lift $ doPhysicsTick pm' st
			    modify $ \ps -> ps {psLastAlerts = alerts}
			    pushPState st'

	edit :: UIMonad uiM => StateT EditState uiM ()
	edit = do
	    es <- get
	    st:_ <- gets esGameStateStack
	    case esSwitchMode es of
		Just GMQuit -> return ()
		Just GMPlay -> subPlay $ head $ esGameStateStack es
		_ -> do
		    lift $ drawEditState es
		    cmds <- lift $ getInput IMEdit
		    sequence_ $ map processEditCommand cmds
		    edit
	    where
		doForce force = do
		    st:_ <- gets esGameStateStack
		    let (st',alerts) = runWriter $ resolveForces (Vector.singleton force) Vector.empty st
		    lift (drawAlerts alerts) >> pushEState st'
		processEditCommand CmdQuit = do
		    really <- lift $ confirm "Really quit?"
		    when really $ modify $ \es -> es {esSwitchMode = Just GMQuit}
		processEditCommand CmdPlay =
		    modify $ \es -> es {esSwitchMode = Just GMPlay }
		processEditCommand CmdResetTools = modifyEState canonify
		processEditCommand CmdUndo = do
		    st:sts <- gets esGameStateStack
		    usts <- gets esUndoneStack
		    when (not $ null sts) $ modify $ \es -> es {esGameStateStack = sts, esUndoneStack = st:usts}
		processEditCommand CmdRedo = do
		    usts <- gets esUndoneStack
		    case usts of 
			[] -> return ()
			ust:usts' -> do
			    pushEState ust
			    modify $ \es -> es {esUndoneStack = usts'}
		processEditCommand CmdUnselect =
		    modify $ \es -> es {selectedPiece = Nothing}
		processEditCommand CmdSelect = do
		    selPiece <- gets selectedPiece
		    selPos <- gets selectedPos
		    st:_ <- gets esGameStateStack
		    let selPiece' =
			    if isJust selPiece
				then Nothing
				else liftM fst $ Map.lookup selPos $ stateBoard st
		    modify $ \es -> es {selectedPiece = selPiece'}
		processEditCommand (CmdDir _ dir) = do
		    selPos <- gets selectedPos
		    selPiece <- gets selectedPiece
		    case selPiece of
			Nothing -> modify $ \es -> es {selectedPos = checkBounds selPos $ dir <+> selPos}
			Just p -> doForce $ Push p dir
		processEditCommand (CmdMoveTo newPos) = do
		    selPos <- gets selectedPos
		    modify $ \es -> es {selectedPos = truncateToBounds newPos}
		processEditCommand (CmdRotate dir) = do
		    selPiece <- gets selectedPiece
		    case selPiece of
			Nothing -> return ()
			Just p -> doForce $ Torque p dir
		processEditCommand (CmdTile tile) = drawTile tile False
		processEditCommand (CmdPaint tile) = drawTile tile True
		processEditCommand CmdMerge = do
		    selPos <- gets selectedPos
		    st:_ <- gets esGameStateStack
		    cmds <- lift $ getInput IMEdit
		    case cmds of
			[CmdDir _ mergeDir] -> modifyEState $ mergeTiles selPos mergeDir True
			_ -> return ()
		processEditCommand CmdWait = do
		    selPos <- gets selectedPos
		    last <- gets lastModPos
		    selPiece <- gets selectedPiece
		    st:_ <- gets esGameStateStack
		    case selPiece of
			Nothing -> do
			    modifyEState $ modTile Nothing selPos last False
			    modify $ \es -> es {lastModPos = selPos}
			Just p -> do (st',_) <- lift $ doPhysicsTick NullPM st
				     pushEState st'
		processEditCommand CmdDelete = do
		    selPiece <- gets selectedPiece
		    st:_ <- gets esGameStateStack
		    case selPiece of
			Nothing -> return ()
			Just p -> do modify $ \es -> es {selectedPiece = Nothing}
				     modifyEState $ delPiece p
		processEditCommand CmdWriteState = do
		    st:_ <- gets esGameStateStack
		    let wfname = fromMaybe "unnamed.lock" fname 
		    liftIO $ writeFile wfname $ show (frame, canonify st)
		processEditCommand _ = return ()
		drawTile tile painting = do
		    modify $ \es -> es {selectedPiece = Nothing}
		    selPos <- gets selectedPos
		    last <- gets lastModPos
		    modifyEState $ modTile (Just tile) selPos last painting
		    modify $ \es -> es {lastModPos = selPos}
		subPlay :: UIMonad uiM => GameState -> StateT EditState uiM ()
		subPlay st = do
		    modify $ \es -> es {esSwitchMode = Nothing}
		    (_,PlayState (st':_) _ _ _ _) <- lift $ runStateT play $ PlayState [st] [] [] False Nothing
		    pushEState st'
		    edit

	pushEState :: UIMonad uiM => GameState -> StateT EditState uiM ()
	pushEState st = do
	    st':sts <- gets esGameStateStack
	    when (st' /= st) $ modify $ \es -> es {esGameStateStack = st:st':sts, esUndoneStack = []}
	pushPState :: UIMonad uiM => GameState -> StateT PlayState uiM ()
	pushPState st = do
	    st':sts <- gets psGameStateStack
	    when (st' /= st) $ modify $ \ps -> ps {psGameStateStack = st:st':sts, psUndoneStack = []}
	modifyEState :: UIMonad uiM => (GameState -> GameState) -> StateT EditState uiM ()
	modifyEState f = do
	    st:_ <- gets esGameStateStack
	    pushEState $ f st
	checkBounds :: HexPos -> HexPos -> HexPos
	checkBounds def pos = if inBounds pos then pos else def
	inBounds :: HexPos -> Bool
	inBounds pos = hexLen (pos <-> origin) < lockBodySize frame
	truncateToBounds :: HexPos -> HexPos
	truncateToBounds pos@(PHS v) =
	    if inBounds pos
		then pos
		else PHS $ scaleToLength ((lockBodySize frame)-1) v
	delOOB :: GameState -> GameState
	delOOB st = case listToMaybe [ idx |
		(idx,_) <- enumVec $ placedPieces st
		, not $ isFrame idx
		, and $ map (not.inBounds) $ fullFootprint st idx ] of
	    Nothing -> st
	    Just idx -> delOOB $ delPiece idx st
	canonify st = addTools $ stabilise $ delTools $ delOOB st
	delTools st = case listToMaybe [ idx | (idx,pp) <- enumVec $ placedPieces st
			      , isTool $ placedPiece pp ] of
			Nothing -> st
			Just idx -> delTools $ delPiece idx st
	addTools st = foldr addpp st $ initTools frame
	stabilise st =
	    let (st',_) = runWriter $ physicsTick NullPM st
	    in if st == st' then st else stabilise st'
    
	doPhysicsTick :: UIMonad uiM => PlayerMove -> GameState -> uiM (GameState, [Alert])
	doPhysicsTick pm st =
	    let r@(st',alerts) = runWriter $ physicsTick pm st in
	    drawAlerts alerts >> return r

	confirm :: UIMonad uiM => String -> uiM Bool
	confirm prompt = do
	    askYesNo prompt
	    waitConfirm
	    where
		waitConfirm = do
		    input <- getInput IMConfirm
		    if CmdYes `elem` input
			then return True
			else if CmdNo `elem` input
			    then return False
			    else waitConfirm

data Opt = LockSize Int | ForceCurses
    deriving (Eq, Ord, Show)
options = [
    Option ['s'] ["locksize"] (ReqArg (LockSize . read) "SIZE") "locksize"
    , Option ['c'] ["curses"] (NoArg ForceCurses) "force curses UI"
    ]
parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
	(o,n,[]) -> return (o,n)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: lock [OPTION...] [file]"

readLockFile :: String -> IO (Maybe Lock)
readLockFile file = do
    lockstr <- liftIO $ catchJust (guard . isDoesNotExistError)
		  (readFile file)
		  (\_ -> return "")
    return $ length lockstr `seq` -- XXX: forces file to be read and closed
	(liftM fst $ listToMaybe $ reads lockstr)
    
main = do
    argv <- getArgs
    (opts,args) <- parseArgs argv
    let frameSize = fromMaybe 8 $ listToMaybe [ size | LockSize size <- opts ]
    let initFrame = BasicFrame frameSize
    let fname = listToMaybe args
    mlock <- case fname of
		Nothing -> return Nothing
		Just fname -> readLockFile fname
    let lock = fromMaybe (initFrame, baseState initFrame) $ mlock
    
    sdlOk <- if ForceCurses `elem` opts
	then return False
	else runUI $ ((test lock fname)::SDLUI.UIM Bool)
    when (not sdlOk) $ void $ runUI $ ((test lock fname)::CursesUI.UIM Bool)
