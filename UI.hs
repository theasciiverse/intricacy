module UI where 

import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Char
import Data.List
import qualified Data.Binary as Bin
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.STM
import Control.Concurrent
import System.Directory
import System.FilePath
import System.IO.Error
import System.Time
import Data.Array

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
import Database
import Protocol
import Metagame
import ServerAddr

class (Applicative m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawMainState :: StateT MainState m ()
    drawAlerts :: [Alert] -> m ()
    drawMessage :: String -> m ()
    drawError :: String -> m ()
    getInput :: InputMode -> m [ Command ]
    unblockInput :: m (IO ())

    doUI :: m a -> IO (Maybe a)
    doUI m = runUI $ do
	ok <- initUI
	if ok then m >>= (endUI >>).return.Just else return Nothing

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
	{ curServer :: ServerAddr
	, undeclareds :: [Undeclared]
	, cacheOnly :: Bool
	, curAuth :: Maybe Auth
	, codenameStack :: [Codename]
	, newAsync :: TVar Bool
	, userInfos :: Map Codename (TVar FetchedRecord, Integer)
	, curLockTVars :: Maybe (Array LockIndex (TVar (Maybe Lock)))
	, curLockPath :: FilePath
	, curLock :: Maybe Lock
	}

newPlayState (frame,st) = PlayState st frame [] False False [] []
newEditState (frame,st) mpath = EditState [st] [] frame mpath Nothing (PHS zero) (PHS zero) 
initMetaState = do
    tvar <- atomically $ newTVar False
    (saddr, auth, path) <- confFilePath "metagame.conf" >>=
	liftM (fromMaybe (defaultServerAddr, Nothing, "")) . readReadFile
    let names = case auth of
	    Nothing -> []
	    Just (Auth name _) -> [name]
    undecls <- confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>=
	liftM (fromMaybe []) . readReadFile
    lock <- readLock path
    return $ MetaState saddr undecls False auth names tvar Map.empty Nothing path lock

readLock :: FilePath -> IO (Maybe Lock)
readLock path = fullLockPath path >>= readReadFile
writeLock path lock = fullLockPath path >>= flip writeReadFile lock
fullLockPath path = if isAbsolute path
    then return path
    else (++(pathSeparator:path)) <$> confFilePath "locks"

writeMetaState (MetaState saddr undecls _ auth _ _ _ _ path _) = do
    confFilePath "metagame.conf" >>= flip writeReadFile (saddr, auth, path)
    confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>= flip writeReadFile undecls

data InputMode = IMEdit | IMPlay | IMMeta | IMTextInput
    deriving (Eq, Ord, Show, Read)

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState _ _ _ _ _ _ _ -> IMPlay
    EditState _ _ _ _ _ _ _ -> IMEdit
    MetaState _ _ _ _ _ _ _ _ _ _ -> IMMeta

instance Error () where noMsg = ()

getTime = do 
    TOD secs msecs <- getClockTime
    return secs

getUInfoFetched :: UIMonad uiM => Integer -> Codename -> StateT MainState uiM FetchedRecord
getUInfoFetched staleTime name = do
    gets ((Map.lookup name) . userInfos) >>=
	\x -> case x of
	    Nothing -> set
	    Just (tvar, time) -> do
		now <- liftIO getTime
		if now - time > staleTime
		    then set
		    else liftIO $ atomically $ readTVar tvar
    where set = do
	    tvar <- setUInfoTVar name
	    liftIO $ atomically $ readTVar tvar
setUInfoTVar name = do
    now <- liftIO getTime
    tvar <- getRecordCachedFromCur True $ RecUserInfo name
    modify $ \ms -> ms {userInfos = Map.insert name (tvar, now) $ userInfos ms}
    return tvar

invalidateUInfo :: UIMonad uiM => Codename -> StateT MainState uiM ()
invalidateUInfo name = 
    modify $ \ms -> ms {userInfos = Map.delete name $ userInfos ms}

setLockTVars name = do
    uiTVar <- setUInfoTVar name
    saddr <- gets curServer
    cOnly <- gets cacheOnly
    flag <- gets newAsync
    lockTVars <- liftM (listArray (0,maxLocks-1)) $ liftIO $ sequence $
	flip map [0..maxLocks-1] $ getLockTVar saddr uiTVar flag cOnly
    modify $ \ms -> ms { curLockTVars = Just lockTVars }
    where
	getLockTVar saddr uiTVar flag cOnly n = do
	    tvar <- atomically $ newTVar Nothing
	    forkIO $ do
		mlockinfo <- atomically $ do
		    uinforc <- readTVar uiTVar
		    case uinforc of
			FetchedRecord _ _ (Just (RCUserInfo uinfo)) ->
			    return $ userLocks uinfo ! n
			_ -> retry
		case mlockinfo of
		    Nothing -> return ()
		    Just lockinfo -> do
			ltvar <- getRecordCached saddr Nothing Nothing cOnly $ RecLock $ lockSpec lockinfo
			atomically $ do
			    lockrc <- readTVar ltvar
			    case lockrc of
				FetchedRecord _ (Just err) _ -> return ()
				FetchedRecord _ _ (Just (RCLock lock)) -> do
				    writeTVar tvar $ Just $ reframe lock
				    writeTVar flag True
				_ -> retry
	    return tvar

refreshUInfoUI :: (UIMonad uiM) => StateT MainState uiM ()
refreshUInfoUI = void.runMaybeT $ do
    MaybeT (listToMaybe <$> gets codenameStack) >>= lift.setLockTVars

curServerAction act = do
    saddr <- gets curServer
    auth <- gets curAuth
    liftIO $ makeRequest saddr $ ClientRequest protocolVersion auth act

getRecordCachedFromCur flagIt rec = do
    saddr <- gets curServer
    auth <- gets curAuth
    cOnly <- gets cacheOnly
    flag <- gets newAsync
    liftIO $ getRecordCached saddr auth
	(if flagIt then Just flag else Nothing) cOnly rec

getRelScore :: (UIMonad uiM) => Codename -> StateT MainState uiM (Maybe Int)
getRelScore name = runMaybeT $ do
    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
    guard $ ourName /= name
    RCUserInfo uinfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 name
    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
    return $ countUnaccessedBy ourUInfo name - countUnaccessedBy uinfo ourName
    where
	countUnaccessedBy (UserInfo _ locks _) name =
	    length $ filter (unaccessedBy name) $ elems locks
	unaccessedBy name Nothing = False
	unaccessedBy name (Just linfo) = not $ public linfo || name `elem` accessedBy linfo 

getNotesReadOn lockinfo = (maybe [] id <$>) $ runMaybeT $ do
    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
    return $ filter (\n -> isNothing (noteBehind n) || n `elem` notesRead ourUInfo) $ lockSolutions lockinfo

interactUI :: UIMonad uiM => StateT MainState uiM ()
interactUI = do
    (IMMeta==) . ms2im <$> get >>= flip when (void $ do
	flag <- gets newAsync
	unblock <- lift unblockInput
	liftIO $ forkIO $ forever $ do
	    atomically $ readTVar flag >>= check >> writeTVar flag False
	    unblock
	ourName <- (authUser <$>) <$> gets curAuth
	maybe (return ()) setLockTVars ourName
	)
    loop

    where
	loop = do
	    mainSt <- get
	    let im = ms2im mainSt
	    when (im == IMPlay) checkWon
	    drawMainState
	    cmds <- lift $ getInput im
	    quit <- liftM (\e -> case e of Left _ -> True; _ -> False) $
		runErrorT $ sequence_ $ map (processCommand im) cmds
	    if quit then return () else loop

	processCommand :: UIMonad uiM => InputMode -> Command -> ErrorT () (StateT MainState uiM) ()
	processCommand im CmdQuit = do
	    when (im == IMPlay) $
		lift (gets psSolved) >>= flip when (throwError ())
	    really <- lift $ lift $ confirm "Really quit?"
	    when really $ throwError ()
	processCommand im cmd = lift $ processCommand' im cmd

	processCommand' :: UIMonad uiM => InputMode -> Command -> StateT MainState uiM ()
	processCommand' IMMeta CmdSelCodename = void.runMaybeT $ do
	    newCodename <- liftM (map toUpper) $ MaybeT $ lift $ textInput "Codename:" 3 True Nothing Nothing
	    guard $ length newCodename == 3
	    lift $ do
		modify $ \ms -> ms { codenameStack = (newCodename:codenameStack ms) }
		refreshUInfoUI
	processCommand' IMMeta CmdHome = void.runMaybeT $ do
	    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
	    lift $ do
		modify $ \ms -> ms { codenameStack = (ourName:codenameStack ms) }
		refreshUInfoUI
	processCommand' IMMeta CmdBackCodename = do
	    stack <- gets codenameStack
	    when (length stack > 1) $ do
		modify $ \ms -> ms { codenameStack = tail stack }
		refreshUInfoUI
	processCommand' IMMeta CmdRegister = void.runMaybeT $ do
	    -- guard =<< isNothing <$> lift (gets curAuth)
	    newName <- MaybeT $ listToMaybe <$> gets codenameStack
	    confirmOrBail $ "Register codename "++newName++"?"
	    passwd <- MaybeT $ lift $ textInput "Enter new password:" 64 False Nothing Nothing
	    lift $ do
		modify $ \ms -> ms {curAuth = Just $ Auth newName passwd}
		resp <- curServerAction $ Register
		case resp of
		    ServerAck -> do
			lift $ drawMessage "Registered!"
			invalidateUInfo newName
		    ServerError err -> do
			lift $ drawMessage err
			modify $ \ms -> ms {curAuth = Nothing}
		    _ -> lift $ drawMessage "BUG"
	processCommand' IMMeta CmdAuth = void.runMaybeT $ do
	    auth <- lift $ gets curAuth
	    name <- MaybeT $ listToMaybe <$> gets codenameStack
	    passwd <- MaybeT $ lift $ textInput ("Enter password for "++name++":") 64 False Nothing Nothing
	    lift $ do
		modify $ \ms -> ms {curAuth = Just $ Auth name passwd}
		resp <- curServerAction $ Authenticate
		case resp of
		    ServerAck -> (lift $ drawMessage "Authenticated.")
		    ServerError err -> do
			lift $ drawMessage err
			modify $ \ms -> ms {curAuth = auth}
		    _ -> lift $ drawMessage "BUG"
	processCommand' IMMeta CmdSolve = void.runMaybeT $ do
	    name <- MaybeT $ listToMaybe <$> gets codenameStack
	    RCUserInfo uinfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 name
	    index <- askLockIndex "Solve which lock?" (\i -> isJust $ userLocks uinfo ! i)
	    ls <- MaybeT $ return $ lockSpec <$> userLocks uinfo ! index
	    undecls <- lift (gets undeclareds)
	    msum [ do
		    undecl <- MaybeT $ return $ find (\(Undeclared _ ls' _) -> ls == ls') undecls
		    MaybeT $ gets curAuth
		    confirmOrBail "Declare existing solution?"
		    void.lift.runMaybeT $ do -- ignores failures
			declare undecl
		, do
		    tvar <- lift $ getRecordCachedFromCur False $ RecLock ls
		    RCLock lock <- MaybeT $ liftIO $ atomically $ do
			FetchedRecord fresh err rc <- readTVar tvar
			check fresh
			return rc
		    soln <- solveLock lock
		    mourName <- lift $ (authUser <$>) <$> gets curAuth
		    guard $ mourName /= Just name
		    let undecl = Undeclared soln ls (ActiveLock name index) 
		    msum [ do
			    MaybeT $ gets curAuth
			    confirmOrBail "Declare solution?"
			    declare undecl
			, unless (any (\(Undeclared _ ls' _) -> ls == ls') undecls) $
			    modify $ \ms -> ms { undeclareds = (undecl : undeclareds ms) }
			]
		]
	processCommand' IMMeta CmdDeclare = void.runMaybeT $ do
	    name <- MaybeT $ listToMaybe <$> gets codenameStack
	    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
	    {-
	    let pertinent = if name == ourName then
		    const True
		    else \(Undeclared _ _ (ActiveLock name' _)) -> name==name'
	    undecls <- lift $ filter pertinent <$> gets undeclareds
	    -}
	    guard $ name == ourName
	    undecls <- lift $ gets undeclareds
	    guard $ not $ null undecls
	    declare =<< if length undecls == 1
		then return $ head undecls
		else do
		    which <- MaybeT $ lift $ textInput
			--("Declare which solution [1-" ++ show (length undecls) ++ "?")
			("Declare which solution?")
			5 False Nothing Nothing
		    MaybeT $ return $ msum
			[ do
			    i <- fst <$> (listToMaybe $ (reads :: ReadS Int) which)
			    guard $ 0 < i && i <= length undecls
			    return $ undecls !! (i-1)
			, do
			    listToMaybe $ [ undecl
				| undecl@(Undeclared _ _ (ActiveLock name' i)) <- undecls
				, or [ take (length which) (name' ++ ":" ++ [lockIndexChar i]) ==
					map toUpper which 
				    , name'==name && [lockIndexChar i] == which
				    ] ]
			]
	processCommand' IMMeta CmdPlaceLock = void.runMaybeT $ do
	    lock <- MaybeT $ gets curLock
	    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
	    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
	    index <- askLockIndex "Place where?" $ const True
	    when (isJust $ userLocks ourUInfo ! index) $
		confirmOrBail "Really retire existing lock?"
	    soln <- solveLock lock
	    lift $ do
		resp <- curServerAction $ SetLock lock index soln
		case resp of
		    ServerError err -> lift $ drawError err
		    _ -> setLockTVars ourName

	processCommand' IMMeta CmdSelectLock = void.runMaybeT $ do
	    lockdir <- liftIO $ confFilePath "locks"
	    paths <- liftIO $ map (drop (length lockdir + 1)) <$> getDirContentsRec lockdir
	    path <- MaybeT $ lift $ textInput "Select lock:" 1024 False (Just paths) Nothing
	    lift $ setLockPath path
	processCommand' IMMeta CmdNextLock = do
	    gets curLockPath >>= liftIO . nextLock True >>= setLockPath
	processCommand' IMMeta CmdPrevLock = do
	    gets curLockPath >>= liftIO . nextLock False >>= setLockPath
	processCommand' IMMeta CmdEdit = void.runMaybeT $ do
	    lock <- msum [ MaybeT $ gets curLock
		, do
		    frame <- BasicFrame <$> msum [ do
			    saddr <- gets curServer
			    tvar <- lift $ getRecordCachedFromCur False RecServerInfo
			    RCServerInfo (ServerInfo size _) <- MaybeT $ liftIO $ atomically $ do
				FetchedRecord fresh _ rc <- readTVar tvar
				check fresh
				return rc
			    return size
			, do
			    sizet <- MaybeT $ lift $ textInput "Lock size: [3-12]" 2 False Nothing Nothing
			    size <- MaybeT $ return $ fst <$> (listToMaybe $ (reads :: ReadS Int) sizet)
			    guard $ 3 <= size && size <= 12
			    return size
			]
		    return (frame, baseState frame)
		]
	    path <- lift $ gets curLockPath
	    newPath <- MaybeT $ lift $ (esPath <$>) $ execStateT interactUI $ newEditState (reframe lock) $ Just path
	    lift $ setLockPath newPath
	    
	processCommand' IMPlay CmdUndo = do
	    st <- gets psCurrentState
	    stack <- gets psGameStateMoveStack
	    ustms <- gets psUndoneStack
	    when (not $ null stack) $ do
		let (st',pm) = head stack
		modify $ \ps -> ps {psCurrentState=st', psGameStateMoveStack = tail stack,
		    psLastAlerts = [], psUndoneStack = (st,pm):ustms}
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
	processCommand' IMEdit CmdWriteState = void.runMaybeT $ do
	    path <- lift $ gets esPath
	    newPath <- MaybeT $ lift $ textInput "Save lock as:" 16 False Nothing path
	    {-
	    ok <- liftIO $ liftM2 (&&)
		    (liftM2 (||) (not <$> doesFileExist newPath) (writable <$> getPermissions newPath))
		    (not <$> doesDirectoryExist newPath)
		`catchIOError` const (return False)
	    when (not ok) $
		(lift.lift) (drawError "Can't write there.") >> mzero
	    -}
	    fullPath <- liftIO $ fullLockPath newPath
	    (liftIO (doesFileExist fullPath `catchIOError` const (return True)) >>=) $ flip when $
		confirmOrBail $ "Really overwrite '"++fullPath++"'?"
	    lift $ do
		st <- gets $ head.esGameStateStack
		frame <- gets esFrame
		merr <- liftIO $ ((writeReadFile fullPath $ deframe $ canonify (frame, st)) >> return Nothing)
		    `catchIOError` (return . Just . show)
		case merr of
		    Nothing -> modify $ \es -> es {esPath = Just newPath}
		    Just err -> lift $ drawError $ "Write failed: "++err
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
	subPlay lock =
	    pushEState =<< psCurrentState <$> (lift $ execStateT interactUI $ newPlayState lock)

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

	nextLock :: Bool -> FilePath -> IO FilePath
	nextLock newer path = do
	    lockdir <- confFilePath "locks"
	    time <- (Just <$> (fullLockPath path >>= getModificationTime)) `catchIOError` const (return Nothing)
	    paths <- getDirContentsRec lockdir
	    maybe path (drop (length lockdir + 1) . fst) . listToMaybe .
		(if newer then id else reverse) . sortBy (\x y -> compare (snd x) (snd y)) .
		filter (maybe (const True)
		    (\x y -> (if newer then (<) else (>)) x (snd y)) time) <$>
		(\p -> (,) p <$> getModificationTime p) `mapM` paths

	setLockPath path = do
	    lock <- liftIO $ readLock path
	    modify $ \ms -> ms {curLockPath = path, curLock = lock}

	solveLock :: UIMonad uiM => Lock -> MaybeT (StateT MainState uiM) Solution
	solveLock lock = do
	    ps <- lift.lift $ execStateT interactUI $ newPlayState $ reframe lock
	    guard $ psSolved ps
	    return $ reverse $ (map snd) $ psGameStateMoveStack ps

	declare undecl@(Undeclared soln ls al) = do
	    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
	    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
	    index <- askLockIndex "Secure behind which lock?" (\i -> isJust $ userLocks ourUInfo ! i)
	    guard $ isJust $ userLocks ourUInfo ! index
	    resp <- lift $ curServerAction $ DeclareSolution soln ls al index
	    case resp of
		ServerAck -> lift $ do
		    modify $ \ms -> ms {undeclareds = delete undecl $ undeclareds ms}
		    mapM_ invalidateUInfo [lockOwner al, ourName]
		ServerError err -> do
		    lift.lift $ drawError err
		    mzero
		_ -> return ()

	askLockIndex prompt check = do
	    let ok = filter check [0,1,2]
	    case length ok of
		0 -> mzero
		1 -> return $ head ok
		_ -> ask ok
	    where
		ask ok = do
		    let prompt' = prompt ++ " [" ++ intersperse ',' (map lockIndexChar ok) ++ "]"
		    index <- MaybeT $ lift $ join . (((charLockIndex<$>).listToMaybe)<$>) <$>
			textInput prompt' 1 True Nothing Nothing
		    if index `elem` ok then return index else ask ok
	confirmOrBail prompt = (guard =<<) $ lift.lift $ confirm prompt
	confirm :: UIMonad uiM => String -> uiM Bool
	confirm prompt = do
	    drawMessage prompt
	    waitConfirm <* drawMessage ""
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

	-- | TODO: draw cursor
	textInput :: UIMonad uiM => String -> Int -> Bool -> Maybe [String] -> Maybe String -> uiM (Maybe String)
	textInput prompt maxlen endOnMax mposss init = do
	    getText (fromMaybe "" init,"") <* drawMessage ""
	    where
		getText (s,stem) = do
		    drawMessage $ prompt ++ " " ++ s
		    if endOnMax && maxlen == length s
		    then return $ Just s
		    else do
			cmds <- getInput IMTextInput
			case foldM applyCmd (s,stem) cmds of
			    Left False -> return Nothing
			    Left True -> return $ Just s
			    Right (s',stem') -> getText (s',stem')
		    where
			applyCmd (s,stem) (CmdInputChar c) = case c of
			    '\ESC' -> Left False
			    '\n' -> Left True
			    '\r' -> Left True
			    '\NAK' -> Right ("","")  -- ^U
			    '\b' -> Right $ (take (length s - 1) s, "")
			    '\DEL' -> Right $ (take (length s - 1) s, "")
			    '\t' -> case mposss of
				Nothing -> Right (s,stem)
				Just possibilities -> if null stem
				    then let
					completions = filter (completes s) possibilities
					pref = if null completions then s else
					    let c = head completions
					    in head [ c' | n <- reverse [0..length c],
						let c'=take n c, all (completes c') completions ]
					in Right (pref,pref)
				    else let
					completions = filter (completes stem) possibilities
					later = filter (>s) completions
					s' = if null completions then s
					    else if null later then head completions
					    else minimum later
					in Right (s',stem)
			    _ -> if isPrint c
				then Right $ ((if length s >= maxlen then id else (++[c])) s, "")
				else Right (s,stem)
			applyCmd _ _ = Left False
		completes s s' = take (length s) s' == s
