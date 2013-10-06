module UI where 

import Control.Monad.Trans
import Control.Monad.State
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Char (ord, isPrint)
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM
import System.Directory

import Hex
import Command
import Mundanities
import GameState
import Physics
import Command
import Frame
import BinaryInstances
import Lock
import Cache
import Protocol
import Metagame

class (Functor m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawMainState :: MainState -> m ()
    drawAlerts :: [Alert] -> m ()
    drawMessage :: String -> m ()
    getInput :: InputMode -> m [ Command ]

    doUI :: m () -> IO Bool
    doUI m = runUI $ do
	ok <- initUI
	if ok then m >> endUI >> return True else return False

-- | this could be neatened using GADTs
data MainState 
    = PlayState
	{ psCurrentState::GameState
	, psFrame::Frame
	, psLastAlerts::[Alert]
	, wrenchSelected::Bool
	, psSolved::Bool
	, psGameStateMoveStack::[(GameState, PlayerMove)]
	, psUndoneStack::[(GameState, PlayerMove)]
	}
    | EditState
	{ esGameStateStack::[GameState]
	, esUndoneStack::[GameState]
	, esFrame::Frame
	, esPath::Maybe FilePath
	, selectedPiece::Maybe PieceIdx
	, selectedPos::HexPos
	, lastModPos::HexPos
	}
    | MetaState
	{ server :: ServerAddr
	, cacheOnly :: Bool
	, auth :: Maybe Auth
	, codenameStack :: [Codename]
	, currentUInfo :: Maybe (TVar FetchedRecord)
	, currentLockPath :: Maybe FilePath
	, currentLock :: Maybe Lock
	}

newPlayState (frame,st) = PlayState st frame [] False False [] []
newEditState (frame,st) mpath = EditState [st] [] frame mpath Nothing (PHS zero) (PHS zero) 
initMetaState = MetaState (ServerAddr "" 0) False Nothing [] Nothing Nothing Nothing

data InputMode = IMEdit | IMPlay | IMMeta | IMTextInput
    deriving (Eq, Ord, Show, Read)

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState _ _ _ _ _ _ _ -> IMPlay
    EditState _ _ _ _ _ _ _ -> IMEdit
    MetaState _ _ _ _ _ _ _ -> IMMeta

instance Error () where noMsg = ()

interactUI :: UIMonad uiM => StateT MainState uiM ()
interactUI = do
    mainSt <- get
    let im = ms2im mainSt
    when (im == IMPlay) checkWon
    lift $ drawMainState mainSt
    cmds <- lift $ getInput im
    quit <- liftM (\e -> case e of Left _ -> True; _ -> False) $
	runErrorT $ sequence_ $ map (processCommand im) cmds
    if quit then return () else interactUI

    where
	processCommand :: UIMonad uiM => InputMode -> Command -> ErrorT () (StateT MainState uiM) ()
	processCommand _ CmdQuit = do
	    really <- lift $ lift $ confirm "Really quit?"
	    when really $ throwError ()
	processCommand im cmd = lift $ processCommand' im cmd
	processCommand' :: UIMonad uiM => InputMode -> Command -> StateT MainState uiM ()
	processCommand' IMMeta CmdSelCodename = void $ runMaybeT $ do
	    newCodename <- MaybeT $ lift $ textInput "Codename:" 3 True
	    lift $ modify $ \ms -> ms { codenameStack = (newCodename:codenameStack ms)
		    , currentUInfo = Nothing}
	    
	processCommand' IMPlay CmdUndo = do
	    st <- gets psCurrentState
	    (st',pm):stms <- gets psGameStateMoveStack
	    ustms <- gets psUndoneStack
	    when (not $ null stms) $ modify $ \ps -> ps {psCurrentState=st', psGameStateMoveStack = stms, psLastAlerts = [], psUndoneStack = (st,pm):ustms}
	processCommand' IMPlay CmdRedo = do
	    ustms <- gets psUndoneStack
	    case ustms of 
		[] -> return ()
		ustm:ustms' -> do
		    pushPState ustm
		    modify $ \ps -> ps {psUndoneStack = ustms'}
	processCommand' IMPlay cmd = do
	    wsel <- gets wrenchSelected
	    st <- gets psCurrentState
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
		    pushPState (st',pm')

	processCommand' IMEdit CmdPlay = do
	    st <- gets $ head.esGameStateStack
	    frame <- gets esFrame
	    subPlay (frame,st)
	processCommand' IMEdit CmdResetTools = do
	    frame <- gets esFrame
	    modifyEState (\st -> snd $ canonify (frame, st))
	processCommand' IMEdit CmdUndo = do
	    st:sts <- gets esGameStateStack
	    usts <- gets esUndoneStack
	    when (not $ null sts) $ modify $ \es -> es {esGameStateStack = sts, esUndoneStack = st:usts}
	processCommand' IMEdit CmdRedo = do
	    usts <- gets esUndoneStack
	    case usts of 
		[] -> return ()
		ust:usts' -> do
		    pushEState ust
		    modify $ \es -> es {esUndoneStack = usts'}
	processCommand' IMEdit CmdUnselect =
	    modify $ \es -> es {selectedPiece = Nothing}
	processCommand' IMEdit CmdSelect = do
	    selPiece <- gets selectedPiece
	    selPos <- gets selectedPos
	    st:_ <- gets esGameStateStack
	    let selPiece' =
		    if isJust selPiece
			then Nothing
			else liftM fst $ Map.lookup selPos $ stateBoard st
	    modify $ \es -> es {selectedPiece = selPiece'}
	processCommand' IMEdit (CmdDir _ dir) = do
	    selPos <- gets selectedPos
	    selPiece <- gets selectedPiece
	    frame <- gets esFrame
	    case selPiece of
		Nothing -> modify $ \es -> es {selectedPos = checkBounds frame selPos $ dir <+> selPos}
		Just p -> doForce $ Push p dir
	processCommand' IMEdit (CmdMoveTo newPos) = do
	    selPos <- gets selectedPos
	    frame <- gets esFrame
	    modify $ \es -> es {selectedPos = truncateToBounds frame newPos}
	processCommand' IMEdit (CmdRotate dir) = do
	    selPiece <- gets selectedPiece
	    case selPiece of
		Nothing -> return ()
		Just p -> doForce $ Torque p dir
	processCommand' IMEdit (CmdTile tile) = drawTile tile False
	processCommand' IMEdit (CmdPaint tile) = drawTile tile True
	processCommand' IMEdit CmdMerge = do
	    selPos <- gets selectedPos
	    st:_ <- gets esGameStateStack
	    cmds <- lift $ getInput IMEdit
	    case cmds of
		[CmdDir _ mergeDir] -> modifyEState $ mergeTiles selPos mergeDir True
		_ -> return ()
	processCommand' IMEdit CmdWait = do
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
	processCommand' IMEdit CmdDelete = do
	    selPiece <- gets selectedPiece
	    st:_ <- gets esGameStateStack
	    case selPiece of
		Nothing -> return ()
		Just p -> do modify $ \es -> es {selectedPiece = Nothing}
			     modifyEState $ delPiece p
	processCommand' IMEdit CmdWriteState = writeState
	    where writeState = do
		    path <- gets esPath
		    case path of
			Nothing -> do
			    newPath <- lift $ textInput "Save lock as:" 16 False
			    modify $ \es -> es {esPath = newPath}
			    when (isJust newPath) writeState
			Just path -> do
			    alreadyExists <- liftIO $ doesFileExist path
			    really <- if alreadyExists
				then lift $ confirm $ "Really overwrite '"++path++"'?"
				else return True
			    when really $ do
			    st <- gets $ head.esGameStateStack
			    frame <- gets esFrame
			    liftIO $ writeFile path $ show $ deframe $ canonify (frame, st)
	processCommand' _ _ = return ()

	checkWon = do
	    st <- gets psCurrentState
	    frame <- gets psFrame
	    when (checkSolved (frame,st)) $ do
		modify $ \ps -> ps {psSolved = True}
		{-
		pms <- (reverse.(map snd)) `liftM` gets psGameStateMoveStack
		let wfname = fromMaybe "unnamed.lock" fname 
		liftIO $ writeFile (wfname++".solution") $ show pms
		liftIO $ B.writeFile (wfname++".solution.bin") $ Bin.encode $ pms
		-}
	doForce force = do
	    st:_ <- gets esGameStateStack
	    let (st',alerts) = runWriter $ resolveForces
		    (Vector.singleton (ForceChoice [force])) Vector.empty
		    (\_ _->False) st
	    lift (drawAlerts alerts) >> pushEState st'
	drawTile tile painting = do
	    modify $ \es -> es {selectedPiece = Nothing}
	    selPos <- gets selectedPos
	    last <- gets lastModPos
	    modifyEState $ modTile (Just tile) selPos last painting
	    modify $ \es -> es {lastModPos = selPos}
	subPlay :: UIMonad uiM => Lock -> StateT MainState uiM ()
	subPlay lock = do
	    st' <- (psCurrentState.snd) `liftM` (lift $ runStateT interactUI $ newPlayState lock)
	    pushEState st'

	pushEState :: UIMonad uiM => GameState -> StateT MainState uiM ()
	pushEState st = do
	    st':sts <- gets esGameStateStack
	    when (st' /= st) $ modify $ \es -> es {esGameStateStack = st:st':sts, esUndoneStack = []}
	pushPState :: UIMonad uiM => (GameState,PlayerMove) -> StateT MainState uiM ()
	pushPState (st,pm) = do
	    st' <- gets psCurrentState
	    stms <- gets psGameStateMoveStack
	    when (st' /= st) $ modify $ \ps -> ps {psCurrentState = st, psGameStateMoveStack = (st',pm):stms, psUndoneStack = []}
	modifyEState :: UIMonad uiM => (GameState -> GameState) -> StateT MainState uiM ()
	modifyEState f = do
	    st:_ <- gets esGameStateStack
	    pushEState $ f st

	doPhysicsTick :: UIMonad uiM => PlayerMove -> GameState -> uiM (GameState, [Alert])
	doPhysicsTick pm st =
	    let r@(st',alerts) = runWriter $ physicsTick pm st in
	    drawAlerts alerts >> return r

	confirm :: UIMonad uiM => String -> uiM Bool
	confirm prompt = do
	    drawMessage prompt
	    waitConfirm
	    where
		waitConfirm = do
		    cmds <- getInput IMTextInput
		    case msum $ map ansOfCmd cmds of
			Just answer -> return answer
			Nothing -> waitConfirm
		ansOfCmd (CmdInputChar 'n') = Just False
		ansOfCmd (CmdInputChar 'N') = Just False
		ansOfCmd (CmdInputChar 'y') = Just True
		ansOfCmd (CmdInputChar 'Y') = Just True
		ansOfCmd (CmdInputChar '\ESC') = Just False
		ansOfCmd _ = Nothing

	textInput :: UIMonad uiM => String -> Int -> Bool -> uiM (Maybe String)
	textInput prompt maxlen endOnMax = do
	    getText ""
	    where
		getText s = do
		    drawMessage $ prompt ++ " " ++ s ++ (replicate (maxlen-length s) ' ')
		    if endOnMax && maxlen == length s
		    then return $ Just s
		    else do
			cmds <- getInput IMTextInput
			case foldM applyCmd s cmds of
			    Left False -> return Nothing
			    Left True -> return $ Just s
			    Right s' -> getText s'
		    where
			applyCmd s (CmdInputChar c) = case c of
			    '\ESC' -> Left False
			    '\n' -> Left True
			    '\r' -> Left True
			    '\NAK' -> Right []  -- ^U
			    '\b' -> Right $ take (length s - 1) s
			    _ -> if isPrint c
				then Right $ (if length s >= maxlen then id else (++[c])) s
				else Right s
			applyCmd s _ = Left False
