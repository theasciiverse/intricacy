module Interact (interactUI) where 

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
import Control.Concurrent.STM
import Control.Concurrent
import System.Directory
import System.FilePath
import Data.Array
import Data.Function (on)

import Hex
import Command
import Physics
import Mundanities
import AsciiLock
import GameState
import GameStateTypes
import EditGameState
import Frame
import Lock
import Cache
import Database
import Protocol
import Metagame
import ServerAddr
import MainState
import InputMode
import Maxlocksize
import InteractUtil

interactUI :: UIMonad uiM => StateT MainState uiM InteractSuccess
interactUI = do
    lift $ drawMessage ""
    (IMMeta==) . ms2im <$> get >>= flip when (void $ do
	flag <- gets newAsync
	unblock <- lift unblockInput
	_ <- liftIO $ forkIO $ forever $ do
	    atomically $ readTVar flag >>= check >> writeTVar flag False
	    unblock
	testAuth
	refreshUInfoUI
	)
    loop

    where
	loop = do
	    mainSt <- get
	    let im = ms2im mainSt
	    when (im == IMPlay && not (psSolved mainSt)) checkWon
	    when (im == IMMeta) checkAsyncErrors
	    drawMainState
	    cmds <- lift $ getInput im
	    runErrorT (mapM_ (processCommand im) cmds) >>= \x -> case x of
		Left ret -> do
		    lift $ drawMessage ""
		    return ret
		Right _ -> loop

-- | TODO: neater would be to use a stream (see package 'pipes') for
-- input?
getSomeInput im = do
    cmds <- getInput im
    if null cmds then getSomeInput im else return cmds

newtype InteractSuccess = InteractSuccess Bool
instance Error InteractSuccess where
    noMsg = InteractSuccess False

processCommand :: UIMonad uiM => InputMode -> Command -> ErrorT InteractSuccess (StateT MainState uiM) ()
processCommand im CmdQuit = do
    case im of
	IMReplay -> throwError $ InteractSuccess False
	IMPlay -> lift (gets psIsSub) >>= flip when (throwError $ InteractSuccess False)
	IMEdit -> editStateUnsaved <$> get >>= flip unless (throwError $ InteractSuccess True)
	_ -> return ()
    really <- lift $ lift $ confirm "Really quit?"
    when really $ throwError $ InteractSuccess False
processCommand IMPlay CmdOpen = do
    st <- gets psCurrentState
    frame <- gets psFrame
    if checkSolved (frame,st) then throwError $ InteractSuccess True else lift.lift $ drawError "Locked!"
processCommand im cmd = lift $ processCommand' im cmd

processCommand' :: UIMonad uiM => InputMode -> Command -> StateT MainState uiM ()
processCommand' im CmdHelp = lift $ showHelp im
processCommand' _ CmdSuspend = lift $ suspend
processCommand' _ CmdRedraw = lift $ redraw
processCommand' IMMeta (CmdSelCodename mname) = void.runMaybeT $ do
    name <- msum [ MaybeT $ return $ mname
	, do
	    newCodename <- (map toUpper <$>) $ MaybeT $ lift $ textInput "Codename:" 3 False True Nothing Nothing
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
    newSaddrstr <- MaybeT $ lift $ textInput "Set server:" 256 False False
	    (Just $ map saddrStr saddrs) (Just $ saddrStr saddr)
    newSaddr <- MaybeT $ return $ strToSaddr newSaddrstr
    modify $ \ms -> ms { curServer = newSaddr }
    unless (nullSaddr newSaddr) $
	msum [ void $ MaybeT $ getFreshRecBlocking RecServerInfo
	, modify $ \ms -> ms { curServer = saddr }
	]
processCommand' IMMeta CmdToggleCacheOnly =
    not <$> gets cacheOnly >>= \c -> modify $ \ms -> ms {cacheOnly = c}

processCommand' IMMeta CmdRegister = void.runMaybeT $ do
    newName <- mgetCurName
    mauth <- gets curAuth
    let isUs = maybe False ((==newName).authUser) mauth
    confirmOrBail $ if isUs then "Really reset password?" else "Register codename "++newName++"?"
    passwd <- (hashPassword newName <$>) $ MaybeT $ lift $
	textInput "Enter new password:" 64 True False Nothing Nothing
    lift $ if isUs then do
	    resp <- curServerAction $ ResetPassword passwd
	    case resp of
		ServerAck -> do
		    lift $ drawMessage "New password set."
		    modify $ \ms -> ms {curAuth = Just $ Auth newName passwd}
		ServerError err -> lift $ drawMessage err
		_ -> lift $ drawMessage $ "Bad server response: " ++ show resp
	else do
	    modify $ \ms -> ms {curAuth = Just $ Auth newName passwd}
	    resp <- curServerAction Register
	    case resp of
		ServerAck -> do
		    invalidateUInfo newName
		    refreshUInfoUI
		    lift $ drawMessage "Registered!"
		ServerError err -> do
		    lift $ drawMessage err
		    modify $ \ms -> ms {curAuth = Nothing}
		_ -> lift $ drawMessage $ "Bad server response: " ++ show resp
processCommand' IMMeta CmdAuth = void.runMaybeT $ do
    auth <- lift $ gets curAuth
    name <- mgetCurName
    passwd <- (hashPassword name <$>) $ MaybeT $ lift $
	textInput ("Enter password for "++name++":") 64 True False Nothing Nothing
    lift $ do
	modify $ \ms -> ms {curAuth = Just $ Auth name passwd}
	resp <- curServerAction $ Authenticate
	case resp of
	    ServerAck -> (lift $ drawMessage "Authenticated.")
	    ServerError err -> do
		lift $ drawMessage err
		modify $ \ms -> ms {curAuth = auth}
	    _ -> lift $ drawMessage $ "Bad server response: " ++ show resp
processCommand' IMMeta (CmdSolve midx) = void.runMaybeT $ do
    name <- mgetCurName
    uinfo <- mgetUInfo name
    idx <- msum [ MaybeT $ return midx
	, askLockIndex "Solve which lock?" "No lock to solve!" (\i -> isJust $ userLocks uinfo ! i) ]
    ls <- MaybeT $ return $ lockSpec <$> userLocks uinfo ! idx
    undecls <- lift (gets undeclareds)
    msum [ do
	    undecl <- MaybeT $ return $ find (\(Undeclared _ ls' _) -> ls == ls') undecls
	    _ <- MaybeT $ gets curAuth
	    confirmOrBail "Declare existing solution?"
	    void.lift.runMaybeT $ -- ignores MaybeT failures
		declare undecl
	, do
	    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
	    soln <- solveLock lock $ Just $
		"Solving " ++ name ++ ":" ++ [lockIndexChar idx] ++ "  (#" ++ show ls ++")"
	    mourName <- lift $ (authUser <$>) <$> gets curAuth
	    guard $ mourName /= Just name
	    let undecl = Undeclared soln ls (ActiveLock name idx) 
	    msum [ do
		    _ <- MaybeT $ gets curAuth
		    confirmOrBail "Declare solution?"
		    declare undecl
		, unless (any (\(Undeclared _ ls' _) -> ls == ls') undecls) $
		    modify $ \ms -> ms { undeclareds = (undecl : undeclareds ms) }
		]
	]
processCommand' IMMeta (CmdPlayLockSpec mls) = void.runMaybeT $ do
    ls <- msum [ MaybeT $ return mls
	, do 
	    tls <- MaybeT . lift $ textInput "Lock number:" 16 False False Nothing Nothing
	    MaybeT . return $ fst <$> (listToMaybe $ (reads :: ReadS Int) tls)
	]
    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
    solveLock lock $ Just $ "Solving " ++ show ls
    
processCommand' IMMeta (CmdDeclare mundecl) = void.runMaybeT $ do
    name <- mgetCurName
    ourName <- mgetOurName
    guard $ name == ourName
    undecls <- lift $ gets undeclareds
    guard $ not $ null undecls
    declare =<< msum [ MaybeT $ return mundecl
	, if length undecls == 1
	then return $ head undecls
	else do
	    which <- MaybeT $ lift $ textInput
		("Declare which solution?")
		5 False True Nothing Nothing
	    MaybeT $ return $ msum
		[ do
		    i <- fst <$> (listToMaybe $ (reads :: ReadS Int) which)
		    guard $ 0 < i && i <= length undecls
		    return $ undecls !! (i-1)
		, listToMaybe $ [ undecl
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
	idx <- askLockIndex "View solution to which lock?" "No solutions to view" $ not.null.(noteses!!)
	let notes = noteses!!idx
	    authors = map noteAuthor notes
	author <- if length notes == 1
	    then return $ noteAuthor $ head notes 
	    else (map toUpper <$>) $ MaybeT $ lift $
		textInput ("View solution by which player? ["
		    ++ intercalate "," (take 3 authors)
		    ++ if length authors > 3 then ",...]" else "]")
		3 False True (Just $ authors) Nothing
	MaybeT $ return $ find ((==author).noteAuthor) notes
	]
    let ActiveLock name idx = noteOn note
    uinfo <- mgetUInfo name
    ls <- lockSpec <$> MaybeT (return $ userLocks uinfo ! idx)
    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
    RCSolution soln <- MaybeT $ getFreshRecBlocking $ RecNote note
    lift.lift $ execStateT interactUI $ newReplayState (snd.reframe$lock) soln $ Just $
	"Viewing solution by " ++ noteAuthor note ++ " to " ++ name ++ [':',lockIndexChar idx]

processCommand' IMMeta (CmdPlaceLock midx) = void.runMaybeT $ do
    ourName <- mgetOurName
    mgetCurName >>= guard . (== ourName)
    lock <- MaybeT $ gets curLock
    ourUInfo <- mgetUInfo ourName
    idx <- msum [ MaybeT $ return midx
	, askLockIndex "Place where?" "bug" $ const True
	]
    when (isJust $ userLocks ourUInfo ! idx) $
	confirmOrBail "Really retire existing lock?"
    soln <- solveLock lock $ Just $ "Prove your lock is solvable."
    lift $ do 
	curServerActionAsync $ SetLock lock idx soln
	invalidateUInfo ourName
	refreshUInfoUI

processCommand' IMMeta CmdSelectLock = void.runMaybeT $ do
    lockdir <- liftIO $ confFilePath "locks"
    paths <- liftIO $ map (drop (length lockdir + 1)) <$> getDirContentsRec lockdir
    path <- MaybeT $ lift $ textInput "Select lock:" 1024 False False (Just paths) Nothing
    lift $ setLockPath path
processCommand' IMMeta CmdNextLock =
    gets curLockPath >>= liftIO . nextLock True >>= setLockPath
processCommand' IMMeta CmdPrevLock =
    gets curLockPath >>= liftIO . nextLock False >>= setLockPath
processCommand' IMMeta CmdNextPage =
    modify $ \ms -> ms { listOffset = listOffset ms + 1 }
processCommand' IMMeta CmdPrevPage =
    modify $ \ms -> ms { listOffset = max 0 $ listOffset ms - 1 }
processCommand' IMMeta CmdEdit = void.runMaybeT $ do
    lock <- msum [ MaybeT $ gets curLock
	, do
	    frame <- BasicFrame <$> msum [ do
		    _ <- gets curServer
		    RCServerInfo (ServerInfo size _) <- MaybeT $ getFreshRecBlocking RecServerInfo
		    return size
		, do
		    sizet <- MaybeT $ lift $ textInput
			("Lock size: [3-" ++ show maxlocksize ++ "]") 2 False False Nothing Nothing
		    size <- MaybeT $ return $ fst <$> (listToMaybe $ (reads :: ReadS Int) sizet)
		    guard $ 3 <= size && size <= maxlocksize
		    return size
		]
	    return (frame, baseState frame)
	]
    path <- lift $ gets curLockPath
    newPath <- MaybeT $ lift $ (esPath <$>) $ execStateT interactUI $ newEditState (reframe lock)
	(if null path then Nothing else Just path)
    lift $ setLockPath newPath
processCommand' IMMeta CmdTutorials = void.runMaybeT $ do
    tutdir <- liftIO $ getDataPath "tutorial"
    tuts <- liftIO $ (sort . filter ((>2).length)) <$> getDirectoryContents tutdir `catchIO` (const $ return [])
    when (null tuts) $ do
	lift.lift $ drawError "No tutorial levels found"
	mzero
    s <- MaybeT $ lift $ textInput ("Play which tutorial level [1-"++show (length tuts)++"]")
	    (length (show (length tuts))) False True Nothing Nothing
    i <- MaybeT $ return $ fst <$> (listToMaybe $ (reads :: ReadS Int) s)
    guard $ 1 <= i && i <= length tuts
    let dotut i = do
	let name = tuts !! (i-1)
	lock <- MaybeT $ liftIO $ readLock (tutdir ++ [pathSeparator] ++ name)
	_ <- solveLock lock $ Just $ "Tutorial: "++ name
	guard $ i+1 <= length tuts
	confirmOrBail $ "Tutorial level completed! Play next tutorial level (" ++ show (i+1) ++ ")?"
	dotut $ i+1
    dotut i
processCommand' IMMeta CmdShowRetired = void.runMaybeT $ do
    name <- mgetCurName
    newRL <- lift (gets retiredLocks) >>= \rl -> case rl of
	Nothing -> do
	    RCLockSpecs lss <- MaybeT $ getFreshRecBlocking $ RecRetiredLocks name
	    return $ Just lss
	Just _ -> return Nothing
    lift $ modify $ \ms -> ms {retiredLocks = newRL}
    
processCommand' IMPlay CmdUndo = do
    st <- gets psCurrentState
    stack <- gets psGameStateMoveStack
    ustms <- gets psUndoneStack
    unless (null stack) $ do
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
    modify $ \es -> es {selectedPiece = Nothing}
    subPlay (frame,st)
processCommand' IMEdit CmdResetTools = do
    frame <- gets esFrame
    modifyEState (\st -> snd $ canonify (frame, st))
processCommand' IMEdit CmdUndo = do
    st:sts <- gets esGameStateStack
    usts <- gets esUndoneStack
    unless (null sts) $ modify $ \es -> es {esGameStateStack = sts, esUndoneStack = st:usts}
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
    frame <- gets esFrame
    modify $ \es -> es {selectedPos = truncateToEditable frame newPos}
processCommand' IMEdit (CmdRotate dir) = do
    selPiece <- gets selectedPiece
    case selPiece of
	Nothing -> return ()
	Just p -> doForce $ Torque p dir
processCommand' IMEdit (CmdTile tile) = do
    selPos <- gets selectedPos
    drawTile selPos (Just tile) False
processCommand' IMEdit (CmdPaint tile) = do
    selPos <- gets selectedPos
    drawTile selPos tile True
processCommand' IMEdit (CmdPaintFromTo tile from to) = do
    frame <- gets esFrame
    paintTilePath tile (truncateToEditable frame from) (truncateToEditable frame to)
processCommand' IMEdit CmdMerge = do
    selPos <- gets selectedPos
    st:_ <- gets esGameStateStack
    lift $ drawMessage "Merge in which direction?"
    cmd <- lift $ head <$> getSomeInput IMEdit
    lift $ drawMessage ""
    case cmd of
	CmdDir _ mergeDir -> do
	    modifyEState $ mergeTiles selPos mergeDir True
	    -- XXX: merging might invalidate selectedPiece
	    modify $ \es -> es {selectedPiece = Nothing}
	_ -> return ()
processCommand' IMEdit CmdMark = void.runMaybeT $ do
    str <- MaybeT $ lift $ textInput "Mark: " 1 False True Nothing Nothing
    ch <- MaybeT $ return $ listToMaybe str
    st:_ <- lift $ gets esGameStateStack
    lift $ modify $ \es -> es {esMarks = Map.insert ch st $ esMarks es}
processCommand' IMEdit CmdJumpMark = void.runMaybeT $ do
    marks <- gets esMarks
    str <- MaybeT $ lift $ textInput "Mark: " 1 False True (Just $ map (:[]) $ Map.keys marks) Nothing
    ch <- MaybeT $ return $ listToMaybe str
    st <- MaybeT $ return $ Map.lookup ch marks
    lift $ pushEState st
processCommand' IMEdit CmdWait = do
    selPos <- gets selectedPos
    selPiece <- gets selectedPiece
    st:_ <- gets esGameStateStack
    case selPiece of
	Nothing -> drawTile selPos Nothing False
	Just _ -> do (st',_) <- lift $ doPhysicsTick NullPM st
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
    newPath <- MaybeT $ lift $ textInput "Save lock as:" 1024 False False Nothing path
    fullPath <- liftIO $ fullLockPath newPath
    (liftIO (doesFileExist fullPath `catchIO` const (return True)) >>=) $ flip when $
	confirmOrBail $ "Really overwrite '"++fullPath++"'?"
    lift $ do
	st <- gets $ head.esGameStateStack
	frame <- gets esFrame
	merr <- liftIO $ ((writeAsciiLockFile fullPath $ canonify (frame, st)) >> return Nothing)
	    `catchIO` (return . Just . show)
	modify $ \es -> es {lastSavedState = Just st}
	case merr of
	    Nothing -> modify $ \es -> es {esPath = Just newPath}
	    Just err -> lift $ drawError $ "Write failed: "++err
processCommand' _ _ = return ()

hashPassword :: String -> String -> String
-- ^ salt password
hashPassword name password = hash $ "IY" ++ name ++ password

subPlay :: UIMonad uiM => Lock -> StateT MainState uiM ()
subPlay lock =
    pushEState =<< psCurrentState <$> (lift $ execStateT interactUI $ newPlayState lock Nothing True)

solveLock :: UIMonad uiM => Lock -> Maybe String -> MaybeT (StateT MainState uiM) Solution
solveLock lock title = do
    (InteractSuccess solved, ps) <- lift.lift $ runStateT interactUI $ newPlayState (reframe lock) title False
    guard $ solved
    return $ reverse $ (map snd) $ psGameStateMoveStack ps
