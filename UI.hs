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
import Data.Time.Clock
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
    showHelp :: InputMode -> m ()
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
	, psTitle::Maybe String
	, psIsSub::Bool
	}
    | ReplayState
	{ rsCurrentState::GameState
	, rsMoveStack::[PlayerMove]
	, rsGameStateMoveStack::[(GameState, PlayerMove)]
	, rsTitle::Maybe String
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
	, randomCodenames :: TVar [Codename]
	, userInfos :: Map Codename (TVar FetchedRecord, UTCTime)
	, curLockTVars :: Maybe (Array LockIndex (TVar (Maybe Lock)))
	, curLockPath :: FilePath
	, curLock :: Maybe Lock
	}

-- | Locks bigger than 11 won't fit in an 80x25 terminal
maxlocksize = 11

newPlayState (frame,st) title sub = PlayState st frame [] False False [] [] title sub
newReplayState st soln title = ReplayState st soln [] title
newEditState (frame,st) mpath = EditState [st] [] frame mpath Nothing (PHS zero) (PHS zero) 
initMetaState = do
    flag <- atomically $ newTVar False
    rnamestvar <- atomically $ newTVar []
    (saddr, auth, path) <- confFilePath "metagame.conf" >>=
	liftM (fromMaybe (defaultServerAddr, Nothing, "")) . readReadFile
    let names = maybeToList $ authUser <$> auth
    undecls <- if nullSaddr saddr then return [] else
	confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>=
	liftM (fromMaybe []) . readReadFile
    lock <- readLock path
    return $ MetaState saddr undecls False auth names flag rnamestvar Map.empty Nothing path lock

readLock :: FilePath -> IO (Maybe Lock)
readLock path = fullLockPath path >>= readReadFile
writeLock path lock = fullLockPath path >>= flip writeReadFile lock
fullLockPath path = if isAbsolute path
    then return path
    else (++(pathSeparator:path)) <$> confFilePath "locks"

writeMetaState (MetaState saddr undecls _ auth _ _ _ _ _ path _) = do
    confFilePath "metagame.conf" >>= flip writeReadFile (saddr, auth, path)
    when (not $ nullSaddr saddr) $
	confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>= flip writeReadFile undecls

data InputMode = IMEdit | IMPlay | IMReplay | IMMeta | IMTextInput
    deriving (Eq, Ord, Show, Read)

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState _ _ _ _ _ _ _ _ _ -> IMPlay
    ReplayState _ _ _ _ -> IMReplay
    EditState _ _ _ _ _ _ _ -> IMEdit
    MetaState _ _ _ _ _ _ _ _ _ _ _ -> IMMeta

instance Error () where noMsg = ()

mgetOurName :: (UIMonad uiM) => MaybeT (StateT MainState uiM) Codename
mgetOurName = MaybeT $ (authUser <$>) <$> gets curAuth
mgetCurName :: (UIMonad uiM) => MaybeT (StateT MainState uiM) Codename
mgetCurName = MaybeT $ listToMaybe <$> gets codenameStack 

getUInfoFetched :: UIMonad uiM => Integer -> Codename -> StateT MainState uiM FetchedRecord
getUInfoFetched staleTime name = do
    gets ((Map.lookup name) . userInfos) >>=
	\x -> case x of
	    Nothing -> set
	    Just (tvar, time) -> do
		now <- liftIO getCurrentTime
		if floor (diffUTCTime now time) > staleTime
		    then set
		    else liftIO $ atomically $ readTVar tvar
    where set = do
	    tvar <- setUInfoTVar name
	    liftIO $ atomically $ readTVar tvar

mgetUInfo :: UIMonad uiM => Codename -> MaybeT (StateT MainState uiM) UserInfo
mgetUInfo name = do
    RCUserInfo (_,uinfo) <- MaybeT $ (fetchedRC <$>) $ getUInfoFetched defaultStaleTime name
    return uinfo
    where defaultStaleTime = 300

setUInfoTVar name = do
    now <- liftIO getCurrentTime
    tvar <- getRecordCachedFromCur True $ RecUserInfo name
    modify $ \ms -> ms {userInfos = Map.insert name (tvar, now) $ userInfos ms}
    return tvar

invalidateUInfo :: UIMonad uiM => Codename -> StateT MainState uiM ()
invalidateUInfo name = 
    modify $ \ms -> ms {userInfos = Map.delete name $ userInfos ms}

setLockTVars name = do
    uiTVar <- setUInfoTVar name
    liftIO $ waitFetchedFresh uiTVar
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
			FetchedRecord _ _ (Just (RCUserInfo (_,uinfo))) ->
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
    name <- mgetCurName
    lift $ setLockTVars name
    ourName <- mgetOurName
    when (ourName == name) $ getRandomNames >> lift purgeInvalidUndecls
    where
	getRandomNames = do
	    rnamestvar <- gets randomCodenames
	    flag <- gets newAsync
	    saddr <- gets curServer
	    liftIO $ forkIO $ do
		resp <- makeRequest saddr $
		    ClientRequest protocolVersion Nothing $ GetRandomNames 30
		case resp of
		    ServedRandomNames names -> atomically $ do 
			    writeTVar rnamestvar names
			    writeTVar flag True
		    _ -> return ()
	purgeInvalidUndecls = do
	    undecls' <- gets undeclareds >>= filterM ((not<$>).invalid)
	    modify $ \ms -> ms { undeclareds = undecls' }
	invalid undecl@(Undeclared _ ls (ActiveLock name index)) =
	    (maybe False id <$>) $ runMaybeT $ do
		uinfo <- mgetUInfo name
		ourName <- mgetOurName
		msum [ do
			linfo <- MaybeT $ return $ userLocks uinfo ! index
			return $ public linfo 
			    || ourName `elem` accessedBy linfo 
			    || lockSpec linfo /= ls
		    , return True
		    ]

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

getFreshRecBlocking rec = do
    tvar <- getRecordCachedFromCur False $ rec
    fetched <- liftIO $ atomically $ do
	fetched@(FetchedRecord fresh _ _) <- readTVar tvar
	check fresh
	return fetched
    case fetchError fetched of
	Nothing -> return $ fetchedRC fetched
	Just err -> lift (drawError err) >> return Nothing

getRelScore :: (UIMonad uiM) => Codename -> StateT MainState uiM (Maybe Int)
getRelScore name = runMaybeT $ do
    ourName <- mgetOurName
    guard $ ourName /= name
    uinfo <- mgetUInfo name
    ourUInfo <- mgetUInfo ourName
    return $ countUnaccessedBy ourUInfo name - countUnaccessedBy uinfo ourName
    where
	countUnaccessedBy (UserInfo _ locks _) name =
	    length $ filter (unaccessedBy name) $ elems locks
	unaccessedBy name Nothing = False
	unaccessedBy name (Just linfo) = not $ public linfo || name `elem` accessedBy linfo 

getNotesReadOn lockinfo = (maybe [] id <$>) $ runMaybeT $ do
    ourName <- mgetOurName
    ourUInfo <- mgetUInfo ourName
    return $ filter (\n -> isNothing (noteBehind n)
	    || n `elem` notesRead ourUInfo) $ lockSolutions lockinfo

interactUI :: UIMonad uiM => StateT MainState uiM ()
interactUI = do
    lift $ drawMessage ""
    (IMMeta==) . ms2im <$> get >>= flip when (void $ do
	flag <- gets newAsync
	unblock <- lift unblockInput
	liftIO $ forkIO $ forever $ do
	    atomically $ readTVar flag >>= check >> writeTVar flag False
	    unblock
	refreshUInfoUI
	)
    loop

    where
	loop = do
	    mainSt <- get
	    let im = ms2im mainSt
	    when (im == IMPlay && not (psSolved mainSt)) checkWon
	    drawMainState
	    cmds <- lift $ getInput im
	    quit <- liftM (\e -> case e of Left _ -> True; _ -> False) $
		runErrorT $ sequence_ $ map (processCommand im) cmds
	    if quit then return () else loop
	getSomeInput im = do
	    cmds <- getInput im
	    if null cmds then getSomeInput im else return cmds

	processCommand :: UIMonad uiM => InputMode -> Command -> ErrorT () (StateT MainState uiM) ()
	processCommand im CmdQuit = do
	    when (im == IMPlay) $
		lift (gets psIsSub) >>= flip when (throwError ())
	    when (im == IMReplay) $ throwError ()
	    really <- lift $ lift $ confirm "Really quit?"
	    when really $ throwError ()
	processCommand IMPlay CmdOpen = do
	    st <- gets psCurrentState
	    frame <- gets psFrame
	    if checkSolved (frame,st) then throwError () else lift.lift $ drawError "Locked!"
	processCommand im cmd = lift $ processCommand' im cmd

	processCommand' :: UIMonad uiM => InputMode -> Command -> StateT MainState uiM ()
	processCommand' im CmdHelp = lift $ showHelp im
	processCommand' IMMeta (CmdSelCodename mname) = void.runMaybeT $ do
	    name <- msum [ MaybeT $ return $ mname
		, do
		    newCodename <- liftM (map toUpper) $ MaybeT $ lift $ textInput "Codename:" 3 True Nothing Nothing
		    guard $ length newCodename == 3
		    return newCodename
		]
	    guard $ validCodeName name
	    lift $ do
		modify $ \ms -> ms { codenameStack = (name:codenameStack ms) }
		refreshUInfoUI
	processCommand' IMMeta CmdHome = void.runMaybeT $ do
	    ourName <- mgetOurName
	    lift $ do
		modify $ \ms -> ms { codenameStack = (ourName:codenameStack ms) }
		refreshUInfoUI
	processCommand' IMMeta CmdBackCodename = do
	    stack <- gets codenameStack
	    when (length stack > 1) $ do
		modify $ \ms -> ms { codenameStack = tail stack }
		refreshUInfoUI
	processCommand' IMMeta CmdSetServer = void.runMaybeT $ do
	    saddr <- gets curServer
	    saddrs <- liftIO $ knownServers
	    newSaddrstr <- MaybeT $ lift $ textInput "Set server:" 256 False
		    (Just $ map saddrStr saddrs) (Just $ saddrStr saddr)
	    newSaddr <- MaybeT $ return $ strToSaddr newSaddrstr
	    modify $ \ms -> ms { curServer = newSaddr }
	    when (not $ nullSaddr newSaddr) $
		msum [ void $ MaybeT $ getFreshRecBlocking RecServerInfo
		, modify $ \ms -> ms { curServer = saddr }
		]

	processCommand' IMMeta CmdRegister = void.runMaybeT $ do
	    -- guard =<< isNothing <$> lift (gets curAuth)
	    newName <- mgetCurName
	    confirmOrBail $ "Register codename "++newName++"?"
	    passwd <- (liftIO . hash =<<) $ MaybeT $ lift $
		textInput "Enter new password:" 64 False Nothing Nothing
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
	    name <- mgetCurName
	    passwd <- (liftIO . hash =<<) $ MaybeT $ lift $
		textInput ("Enter password for "++name++":") 64 False Nothing Nothing
	    lift $ do
		modify $ \ms -> ms {curAuth = Just $ Auth name passwd}
		resp <- curServerAction $ Authenticate
		case resp of
		    ServerAck -> (lift $ drawMessage "Authenticated.")
		    ServerError err -> do
			lift $ drawMessage err
			modify $ \ms -> ms {curAuth = auth}
		    _ -> lift $ drawMessage "BUG"
	processCommand' IMMeta (CmdSolve mindex) = void.runMaybeT $ do
	    name <- mgetCurName
	    uinfo <- mgetUInfo name
	    index <- msum [ MaybeT $ return $ mindex
		, askLockIndex "Solve which lock?" (\i -> isJust $ userLocks uinfo ! i) ]
	    ls <- MaybeT $ return $ lockSpec <$> userLocks uinfo ! index
	    undecls <- lift (gets undeclareds)
	    msum [ do
		    undecl <- MaybeT $ return $ find (\(Undeclared _ ls' _) -> ls == ls') undecls
		    MaybeT $ gets curAuth
		    confirmOrBail "Declare existing solution?"
		    void.lift.runMaybeT $ do -- ignores MaybeT failures
			declare undecl
		, do
		    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
		    soln <- solveLock lock $ Just $ "Solving " ++ name ++ ":" ++ [lockIndexChar index]
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
	processCommand' IMMeta (CmdDeclare mundecl) = void.runMaybeT $ do
	    name <- mgetCurName
	    ourName <- mgetOurName
	    {-
	    let pertinent = if name == ourName then
		    const True
		    else \(Undeclared _ _ (ActiveLock name' _)) -> name==name'
	    undecls <- lift $ filter pertinent <$> gets undeclareds
	    -}
	    guard $ name == ourName
	    undecls <- lift $ gets undeclareds
	    guard $ not $ null undecls
	    declare =<< msum [ MaybeT $ return mundecl
		, if length undecls == 1
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
		]
	processCommand' IMMeta (CmdViewSolution mnote) = void.runMaybeT $ do
	    note <- msum [ MaybeT $ return mnote, do
		ourName <- mgetOurName
		name <- mgetCurName
		uinfo <- mgetUInfo name
		noteses <- lift $ sequence
		    [ case mlockinfo of
			Nothing -> return []
			Just lockinfo -> if ourName == name
			    then return $ lockSolutions lockinfo
			    else do
				ns <- getNotesReadOn lockinfo 
				return $ if length ns < 3 then [] else ns
		    | mlockinfo <- elems $ userLocks uinfo ]
		index <- askLockIndex "View solution to which lock?" $ not.null.(noteses!!)
		let notes = noteses!!index
		    authors = map noteAuthor notes
		author <- if length notes == 1
		    then return $ noteAuthor $ head notes 
		    else (map toUpper <$>) $ MaybeT $ lift $
			textInput ("View solution by which player? ["
			    ++ concat (intersperse "," (take 3 authors))
			    ++ if length authors > 3 then ",...]" else "]")
			3 False (Just $ authors) Nothing
		MaybeT $ return $ find ((==author).noteAuthor) notes
		]
	    let ActiveLock name index = noteOn note
	    uinfo <- mgetUInfo name
	    ls <- lockSpec <$> MaybeT (return $ userLocks uinfo ! index)
	    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
	    RCSolution soln <- MaybeT $ getFreshRecBlocking $ RecNote note
	    lift.lift $ execStateT interactUI $ newReplayState (snd.reframe$lock) soln $ Just $
		"Viewing solution by " ++ noteAuthor note ++ " to " ++ name ++ [':',lockIndexChar index]

	processCommand' IMMeta (CmdPlaceLock mindex) = void.runMaybeT $ do
	    ourName <- mgetOurName
	    name <- mgetCurName
	    guard $ ourName == name
	    lock <- MaybeT $ gets curLock
	    ourUInfo <- mgetUInfo ourName
	    index <- msum [ MaybeT $ return mindex
		, askLockIndex "Place where?" $ const True
		]
	    when (isJust $ userLocks ourUInfo ! index) $
		confirmOrBail "Really retire existing lock?"
	    soln <- solveLock lock $ Just $ "Prove your lock is solvable."
	    lift $ do
		resp <- curServerAction $ SetLock lock index soln
		case resp of
		    ServerError err -> lift $ drawError err
		    _ -> refreshUInfoUI

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
			    RCServerInfo (ServerInfo size _) <- MaybeT $ getFreshRecBlocking RecServerInfo
			    return size
			, do
			    sizet <- MaybeT $ lift $ textInput
				("Lock size: [3-" ++ show maxlocksize ++ "]") 2 False Nothing Nothing
			    size <- MaybeT $ return $ fst <$> (listToMaybe $ (reads :: ReadS Int) sizet)
			    guard $ 3 <= size && size <= maxlocksize
			    return size
			]
		    return (frame, baseState frame)
		]
	    path <- lift $ gets curLockPath
	    newPath <- MaybeT $ lift $ (esPath <$>) $ execStateT interactUI $ newEditState (reframe lock) $ Just path
	    lift $ setLockPath newPath
	processCommand' IMMeta CmdTutorials = void.runMaybeT $ do
	    tutdir <- liftIO $ getDataPath "tutorial"
	    tuts <- liftIO $ (sort . filter ((>2).length)) <$> getDirectoryContents tutdir `catchIOError` (const $ return [])
	    when (null tuts) $ do
		lift.lift $ drawError "No tutorial levels found"
		mzero
	    s <- MaybeT $ lift $ textInput ("Play which tutorial level [1-"++show (length tuts)++"]") 5 False Nothing Nothing
	    i <- MaybeT $ return $ fst <$> (listToMaybe $ (reads :: ReadS Int) s)
	    guard $ 1 <= i && i <= length tuts
	    let dotut i = do
		let name = tuts !! (i-1)
		lock <- MaybeT $ liftIO $ readReadFile (tutdir ++ [pathSeparator] ++ name)
		solveLock lock $ Just $ "Tutorial: "++ name
		guard $ i+1 < length tuts
		confirmOrBail $ "Play tutorial level " ++ show (i+1) ++ "?"
		dotut $ i+1
	    dotut i

	    
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

	processCommand' IMReplay (CmdReplayBack 1) = void.runMaybeT $ do
	    (st',pm) <- MaybeT $ listToMaybe <$> gets rsGameStateMoveStack
	    lift $ modify $ \rs -> rs {rsCurrentState=st'
		, rsGameStateMoveStack = tail $ rsGameStateMoveStack rs
		, rsMoveStack = pm:rsMoveStack rs}
	processCommand' IMReplay (CmdReplayBack n) = replicateM_ n $
	    processCommand' IMReplay (CmdReplayBack 1)
	processCommand' IMReplay (CmdReplayForward 1) = void.runMaybeT $ do
	    pm <- MaybeT $ listToMaybe <$> gets rsMoveStack
	    lift $ do
		st <- gets rsCurrentState
		(st',_) <- lift $ doPhysicsTick pm st
		modify $ \rs -> rs {rsCurrentState = st'
		    , rsGameStateMoveStack = (st,pm):rsGameStateMoveStack rs
		    , rsMoveStack = tail $ rsMoveStack rs}
	processCommand' IMReplay (CmdReplayForward n) = replicateM_ n $
	    processCommand' IMReplay (CmdReplayForward 1)
	processCommand' IMReplay CmdUndo = processCommand' IMReplay (CmdReplayBack 1)
	processCommand' IMReplay CmdRedo = processCommand' IMReplay (CmdReplayForward 1)

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
		lift $ drawMessage "Unlocked!"
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
	    pushEState =<< psCurrentState <$> (lift $ execStateT interactUI $ newPlayState lock Nothing True)

	pushEState :: UIMonad uiM => GameState -> StateT MainState uiM ()
	pushEState st = do
	    st':sts <- gets esGameStateStack
	    when (st' /= st) $ modify $ \es -> es {esGameStateStack = st:st':sts, esUndoneStack = []}
	pushPState :: UIMonad uiM => (GameState,PlayerMove) -> StateT MainState uiM ()
	pushPState (st,pm) = do
	    st' <- gets psCurrentState
	    stms <- gets psGameStateMoveStack
	    when (st' /= st) $ modify $ \ps -> ps {psCurrentState = st,
		    psGameStateMoveStack = (st',pm):stms, psUndoneStack = []}
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
	    time <- (Just <$> (fullLockPath path >>= getModificationTime))
		`catchIOError` const (return Nothing)
	    paths <- getDirContentsRec lockdir
	    maybe path (drop (length lockdir + 1) . fst) . listToMaybe .
		(if newer then id else reverse) . sortBy (\x y -> compare (snd x) (snd y)) .
		filter (maybe (const True)
		    (\x y -> (if newer then (<) else (>)) x (snd y)) time) <$>
		(\p -> (,) p <$> getModificationTime p) `mapM` paths

	setLockPath path = do
	    lock <- liftIO $ readLock path
	    modify $ \ms -> ms {curLockPath = path, curLock = lock}

	solveLock :: UIMonad uiM => Lock -> Maybe String -> MaybeT (StateT MainState uiM) Solution
	solveLock lock title = do
	    ps <- lift.lift $ execStateT interactUI $ newPlayState (reframe lock) title False
	    guard $ psSolved ps
	    return $ reverse $ (map snd) $ psGameStateMoveStack ps

	declare undecl@(Undeclared soln ls al) = do
	    ourName <- mgetOurName
	    ourUInfo <- mgetUInfo ourName
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
		0 -> (lift.lift) (drawError "No appropriate lock is available.") >> mzero
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
			applyCmd x CmdRedraw = Right x
			applyCmd _ _ = Left False
		completes s s' = take (length s) s' == s
