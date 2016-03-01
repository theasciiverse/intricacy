-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Interact (interactUI) where

import Control.Monad.State
import Control.Applicative
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Trans.Except
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
import Safe (readMay)

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
import Util

newtype InteractSuccess = InteractSuccess Bool

interactUI :: UIMonad uiM => MainStateT uiM InteractSuccess
interactUI = do
    im <- gets ms2im
    lift $ onNewMode im
    when (im == IMEdit) setSelectedPosFromMouse
    when (im == IMMeta) $ do
	spawnUnblockerThread

	-- draw before testing auth, lest a timeout mean a blank screen
	drawMainState

	testAuth
	refreshUInfoUI
	tbdg <- lift $ getUIBinding IMMeta CmdTutorials
	isNothing <$> gets curAuth >>?
	    lift $ drawMessage $ "Welcome. To play the tutorial levels, press '"++tbdg++"'."
    setMark False startMark
    interactLoop

    where
	interactLoop = do
	    mainSt <- get
	    let im = ms2im mainSt
	    when (im == IMPlay) checkWon
	    when (im == IMMeta) $ (checkAsync >>) $ void.runMaybeT $
		mourNameSelected >>? lift purgeInvalidUndecls
	    drawMainState
	    cmds <- lift $ getSomeInput im
	    runExceptT (mapM_ (processCommand im) cmds) >>=
		either
		    ((lift (drawMessage "") >>) . return)
		    (const interactLoop)

	-- | unblock input whenever the newAsync TVar is set to True
	spawnUnblockerThread = do
	    flag <- gets newAsync
	    unblock <- lift unblockInput
	    liftIO $ forkIO $ forever $ do
		atomically $ readTVar flag >>= check >> writeTVar flag False
		unblock

runSubMainState :: UIMonad uiM => MainState -> MainStateT uiM (InteractSuccess,MainState)
runSubMainState mSt = lift (runStateT interactUI mSt) <* cleanOnPop
    where cleanOnPop = do
	    im <- gets ms2im
	    lift $ onNewMode im
	    when (im == IMEdit) setSelectedPosFromMouse

execSubMainState :: UIMonad uiM => MainState -> MainStateT uiM MainState
execSubMainState = (snd <$>) . runSubMainState

getSomeInput im = do
    cmds <- getInput im
    if null cmds then getSomeInput im else return cmds

processCommand :: UIMonad uiM => InputMode -> Command -> ExceptT InteractSuccess (MainStateT uiM) ()
processCommand im CmdQuit = do
    case im of
	IMReplay -> throwE $ InteractSuccess False
	IMPlay -> lift (or <$> sequence [gets psIsSub, null <$> gets psGameStateMoveStack])
	    >>? throwE $ InteractSuccess False
	IMEdit -> lift editStateUnsaved >>! throwE $ InteractSuccess True
	_ -> return ()
    title <- lift $ getTitle
    (lift . lift . confirm) ("Really quit"
	    ++ (if im == IMEdit then " without saving" else "")
	    ++ maybe "" (" from "++) title ++ "?")
	>>? throwE $ InteractSuccess False
processCommand im CmdForceQuit = throwE $ InteractSuccess False
processCommand IMPlay CmdOpen = do
    st <- gets psCurrentState
    frame <- gets psFrame
    if checkSolved (frame,st)
	then throwE $ InteractSuccess True
	else lift.lift $ drawError "Locked!"
processCommand im cmd = lift $ processCommand' im cmd

processCommand' :: UIMonad uiM => InputMode -> Command -> MainStateT uiM ()
processCommand' im CmdHelp = lift $
    let showPage p = showHelp im p >>? do
	    void $ textInput "[press a key or RMB]" 1 False True Nothing Nothing
    in sequence_ $ map showPage $ enumFrom HelpPageInput
processCommand' im (CmdBind mcmd)= lift $ (>> endPrompt) $ runMaybeT $ do
    cmd <- liftMaybe mcmd `mplus` do
	lift $ drawPrompt False "Command to bind: "
	msum $ repeat $ do
	    cmd <- MaybeT $ listToMaybe <$> getInput im
	    guard $ not.null $ describeCommand cmd
	    return cmd
    lift $ drawPrompt False ("key to bind to \"" ++ describeCommand cmd ++ "\" (repeat existing user binding to delete): ")
    ch <- MaybeT getChRaw
    guard $ ch /= '\ESC'
    lift $ setUIBinding im cmd ch
processCommand' _ CmdToggleColourMode = lift toggleColourMode
processCommand' _ CmdSuspend = lift suspend
processCommand' _ CmdRedraw = lift redraw
processCommand' im CmdClear = do
    lift $ drawMessage ""
    when (im == IMMeta) $ modify $ \ms -> ms { retiredLocks = Nothing }
processCommand' im CmdMark = void.runMaybeT $ do
    guard $ im `elem` [IMEdit, IMPlay, IMReplay]
    str <- MaybeT $ lift $ textInput "Mark: " 1 False True Nothing Nothing
    ch <- liftMaybe $ listToMaybe str
    guard $ ch `notElem` [startMark, '\'']
    lift $ setMark True ch
processCommand' im CmdJumpMark = void.runMaybeT $ do
    guard $ im `elem` [IMEdit, IMPlay, IMReplay]
    str <- MaybeT $ lift $ textInput
	"Jump to mark (\"'\" to unjump): " 1 False True Nothing Nothing
    ch <- liftMaybe $ listToMaybe str
    lift $ jumpMark ch
processCommand' im CmdReset = jumpMark startMark
processCommand' IMMeta (CmdSelCodename mname) = void.runMaybeT $ do
    mauth <- gets curAuth
    name <- msum [ liftMaybe $ mname
	, do
	    newCodename <- (map toUpper <$>) $ MaybeT $ lift $
		textInput "Select codename:"
		    3 False False Nothing Nothing
	    guard $ length newCodename == 3
	    return newCodename
	]
    guard $ validCodeName name
    lift $ do
	modify $ \ms -> ms { codenameStack = (name:codenameStack ms) }
	invalidateUInfo name
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
    newSaddr <- MaybeT $ ((>>= strToSaddr) <$>) $
	lift $ textInput "Set server:" 256 False False
	    (Just $ map saddrStr saddrs) (Just $ saddrStr saddr)
    modify $ \ms -> ms { curServer = newSaddr }
    guard.not $ nullSaddr newSaddr
    msum [ void.MaybeT $ getFreshRecBlocking RecServerInfo
	, modify (\ms -> ms { curServer = saddr }) >> mzero ]
    lift $ do
	modify $ \ms -> ms {curAuth = Nothing}
	get >>= liftIO . writeServerSolns saddr
	(undecls,partials,_) <- liftIO (readServerSolns newSaddr)
	modify $ \ms -> ms { undeclareds=undecls, partialSolutions=partials }
	rnamestvar <- gets randomCodenames
	liftIO $ atomically $ writeTVar rnamestvar []
	invalidateAllUInfo
	refreshUInfoUI
processCommand' IMMeta CmdToggleCacheOnly =
    not <$> gets cacheOnly >>= \c -> modify $ \ms -> ms {cacheOnly = c}

processCommand' IMMeta CmdRegister = void.runMaybeT $ do
    newName <- mgetCurName
    mauth <- gets curAuth
    let inputPassword = (hashPassword newName <$>) $ MaybeT $ lift $
	    textInput "Enter new password:" 64 True False Nothing Nothing
    let isUs = maybe False ((==newName).authUser) mauth
    if isUs
	then msum [ do
		confirmOrBail "Reset password?"
		passwd <- inputPassword
		lift $ do
		    resp <- curServerAction $ ResetPassword passwd
		    case resp of
			ServerAck -> do
			    lift $ drawMessage "New password set."
			    modify $ \ms -> ms {curAuth = Just $ Auth newName passwd}
			ServerError err -> lift $ drawError err
			_ -> lift $ drawMessage $ "Bad server response: " ++ show resp
	    , do
		confirmOrBail "Configure email notifications?"
		setNotifications
	    ]
	else do
	    passwd <- inputPassword
	    lift $ do
		modify $ \ms -> ms {curAuth = Just $ Auth newName passwd}
		resp <- curServerAction Register
		case resp of
		    ServerAck -> do
			invalidateUInfo newName
			refreshUInfoUI
			conf <- lift $ confirm "Registered! Would you like to be notified by email when someone solves your lock?"
			rbdg <- lift $ getUIBinding IMMeta CmdRegister
			if conf then void $ runMaybeT setNotifications else lift $ drawMessage "Notifications disabled."
		    ServerError err -> do
			lift $ drawError err
			modify $ \ms -> ms {curAuth = Nothing}
		    _ -> lift $ drawMessage $ "Bad server response: " ++ show resp

    where setNotifications = do
	    address <- MaybeT $ lift $ textInput "Enter address, or leave blank to disable notifications:" 128 False False Nothing Nothing
	    lift $ do
		resp <- curServerAction $ SetEmail address
		case resp of
		    ServerAck -> lift $ drawMessage $ if null address then "Notifications disabled." else "Address set."
		    ServerError err -> lift $ drawError err
		    _ -> lift $ drawMessage $ "Bad server response: " ++ show resp

	
processCommand' IMMeta CmdAuth = void.runMaybeT $ do
    auth <- lift $ gets curAuth
    if isJust auth then do
	confirmOrBail "Log out?"
	modify $ \ms -> ms {curAuth = Nothing}
    else do
	name <- mgetCurName
	passwd <- (hashPassword name <$>) $ MaybeT $ lift $
	    textInput ("Enter password for "++name++":") 64 True False Nothing Nothing
	lift $ do
	    modify $ \ms -> ms {curAuth = Just $ Auth name passwd}
	    resp <- curServerAction $ Authenticate
	    case resp of
		ServerAck -> lift $ drawMessage "Authenticated."
		ServerMessage msg -> lift $ drawMessage $ "Server: " ++ msg
		ServerError err -> do
		    lift $ drawError err
		    modify $ \ms -> ms {curAuth = auth}
		_ -> lift $ drawMessage $ "Bad server response: " ++ show resp
	    refreshUInfoUI
processCommand' IMMeta (CmdSolve midx) = void.runMaybeT $ do
    name <- mgetCurName
    uinfo <- mgetUInfo name
    idx <- msum [ liftMaybe midx
	, askLockIndex "Solve which lock?" "No lock to solve!" (\i -> isJust $ userLocks uinfo ! i) ]
    ls <- liftMaybe $ lockSpec <$> userLocks uinfo ! idx
    undecls <- lift (gets undeclareds)
    msum [ do
	    undecl <- liftMaybe $ find (\(Undeclared _ ls' _) -> ls == ls') undecls
	    MaybeT $ gets curAuth
	    confirmOrBail "Declare existing solution?"
	    void.lift.runMaybeT $ -- ignores MaybeT failures
		declare undecl
	, do
	    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
	    mpartial <- Map.lookup ls <$> gets partialSolutions
	    soln <- solveLockSaving ls mpartial False lock $ Just $
		"solving " ++ name ++ ":" ++ [lockIndexChar idx] ++ "  (#" ++ show ls ++")"
	    mourName <- lift $ (authUser <$>) <$> gets curAuth
	    guard $ mourName /= Just name
	    let undecl = Undeclared soln ls (ActiveLock name idx)
	    msum [ do
		    MaybeT $ gets curAuth
		    confirmOrBail "Declare solution?"
		    declare undecl
		, unless (any (\(Undeclared _ ls' _) -> ls == ls') undecls) $
		    modify $ \ms -> ms { undeclareds = (undecl : undeclareds ms) }
		]
	]
processCommand' IMMeta (CmdPlayLockSpec mls) = void.runMaybeT $ do
    ls <- msum [ liftMaybe mls
	, do
	    tls <- MaybeT . lift $ textInput "Lock number:" 16 False False Nothing Nothing
	    liftMaybe $ readMay tls
	]
    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
    solveLock lock $ Just $ "solving " ++ show ls

processCommand' IMMeta (CmdDeclare mundecl) = void.runMaybeT $ do
    guard =<< mourNameSelected
    name <- mgetCurName
    undecls <- lift $ gets undeclareds
    guard $ not $ null undecls
    declare =<< msum [ liftMaybe mundecl
	, if length undecls == 1
	then return $ head undecls
	else do
	    which <- MaybeT $ lift $ textInput
		("Declare which solution?")
		5 False True Nothing Nothing
	    liftMaybe $ msum
		[ do
		    i <- readMay which
		    guard $ 0 < i && i <= length undecls
		    return $ undecls !! (i-1)
		, listToMaybe $
		    [ undecl
		    | undecl@(Undeclared _ _ (ActiveLock name' i)) <- undecls
		    , or
			[ take (length which) (name' ++ ":" ++ [lockIndexChar i]) ==
			    map toUpper which
			, name'==name && [lockIndexChar i] == which
			]
		    ]
		]
	]
processCommand' IMMeta (CmdViewSolution mnote) = void.runMaybeT $ do
    note <- liftMaybe mnote `mplus` do
	ourName <- mgetOurName
	name <- mgetCurName
	uinfo <- mgetUInfo name
	noteses <- lift $ sequence
	    [ case mlockinfo of
		Nothing -> return []
		Just lockinfo -> (++lockSolutions lockinfo) <$> do
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
	liftMaybe $ find ((==author).noteAuthor) notes
    let ActiveLock name idx = noteOn note
    uinfo <- mgetUInfo name
    ls <- lockSpec <$> MaybeT (return $ userLocks uinfo ! idx)
    RCLock lock <- MaybeT $ getFreshRecBlocking $ RecLock ls
    RCSolution soln <- MaybeT $ getFreshRecBlocking $ RecNote note
    lift $ execSubMainState $ newReplayState (snd.reframe$lock) soln $ Just $
	"viewing solution by " ++ noteAuthor note ++ " to " ++ name ++ [':',lockIndexChar idx]

processCommand' IMMeta (CmdPlaceLock midx) = void.runMaybeT $ do
    guard =<< mourNameSelected
    ourName <- mgetOurName
    (lock,msoln) <- MaybeT (gets curLock) `mplus` do
	ebdg <- lift.lift $ getUIBinding IMMeta CmdEdit
	lift.lift $ drawError $ "No lock selected; '"++ebdg++"' to edit one."
	mzero
    lockpath <- lift $ gets curLockPath
    ourUInfo <- mgetUInfo ourName
    idx <- (liftMaybe midx `mplus`) $
	askLockIndex ("Place " ++ show lockpath ++ " in which slot?") "bug" $ const True
    when (isJust $ userLocks ourUInfo ! idx) $
	confirmOrBail "Really retire existing lock?"
    soln <- (liftMaybe msoln `mplus`) $ solveLock lock $ Just $ "testing lock"
    lift $ curServerActionAsyncThenInvalidate
	(SetLock lock idx soln)
	(Just (SomeCodenames [ourName]))

processCommand' IMMeta CmdSelectLock = void.runMaybeT $ do
    lockdir <- liftIO $ confFilePath "locks"
    paths <- liftIO $ map (drop (length lockdir + 1)) <$> getDirContentsRec lockdir
    path <- MaybeT $ lift $ textInput "Lock name:" 1024 False False (Just paths) Nothing
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
    (lock, msoln) <- (MaybeT $ gets curLock) `mplus` do
	size <- msum
	    [ do
		gets curServer
		RCServerInfo (ServerInfo size _) <- MaybeT $ getFreshRecBlocking RecServerInfo
		return size
	    , do
		sizet <- MaybeT $ lift $ textInput
		    ("Lock size: [3-" ++ show maxlocksize ++ "]") 2 False False Nothing Nothing
		size <- liftMaybe $ readMay sizet
		guard $ 3 <= size && size <= maxlocksize
		return size
	    ]
	return (baseLock size, Nothing)
    path <- lift $ gets curLockPath
    newPath <- MaybeT $ (esPath <$>) $ execSubMainState $
	newEditState (reframe lock) msoln (if null path then Nothing else Just path)
    lift $ setLockPath newPath
processCommand' IMMeta CmdTutorials = void.runMaybeT $ do
    tutdir <- liftIO $ getDataPath "tutorial"
    tuts <- liftIO $ (sort . map (takeWhile (/='.')) . filter (isSuffixOf ".lock")) <$>
	getDirectoryContents tutdir `catchIO` (const $ return [])
    when (null tuts) $ do
	lift.lift $ drawError "No tutorial levels found"
	mzero
    -- s <- MaybeT $ lift $ textInput ("Play which tutorial level? [1-"++show (length tuts)++"]")
	    -- (length (show (length tuts))) False True Nothing Nothing
    -- i <- liftMaybe $ readMay s
    -- guard $ 1 <= i && i <= length tuts
    let dotut i msps = do
	let name = tuts !! (i-1)
	let pref = tutdir ++ [pathSeparator] ++ name
	(lock,_) <- MaybeT $ liftIO $ readLock (pref ++ ".lock")
	text <- liftIO $ (fromMaybe "" . listToMaybe) <$> (readStrings (pref ++ ".text") `catchIO` const (return []))
	solveLockSaving i msps True lock $ Just $ "Tutorial " ++ show i ++ ": " ++ text
	if i+1 <= length tuts then do
		-- confirmOrBail $ "Tutorial level completed! Play next tutorial level (" ++ show (i+1) ++ ")?"
		dotut (i+1) Nothing
	    else lift $ do
		modify $ \ms -> ms {tutProgress = (1,Nothing)}
		mauth <- gets curAuth
		cbdg <- lift $ getUIBinding IMMeta $ CmdSelCodename Nothing
		rbdg <- lift $ getUIBinding IMMeta CmdRegister
		if isNothing mauth
		    then do
			let showPage p = lift $ showHelp IMMeta p >>? do
				void $ textInput
				    "[Tutorial completed! Press a key or RMB to continue; you can review this help later with '?']"
				    1 False True Nothing Nothing
			showPage HelpPageGame
			lift $ drawMessage $ 
			    "Tutorial completed! To play on the server, pick a codename ('"++cbdg++
			    "') and register it ('"++rbdg++"')."
		    else lift $ drawMessage $ "Tutorial completed!"
    
    (onLevel,msps) <- lift $ gets tutProgress
    dotut onLevel msps
processCommand' IMMeta CmdShowRetired = void.runMaybeT $ do
    name <- mgetCurName
    newRL <- lift (gets retiredLocks) >>= \rl -> case rl of
	Nothing -> do
	    RCLockSpecs lss <- MaybeT $ getFreshRecBlocking $ RecRetiredLocks name
	    if null lss
		then do
		    lift.lift $ drawError "Player has no retired locks."
		    return Nothing
		else return $ Just lss
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
	ustm@(_,pm):ustms' -> do
	    st <- gets psCurrentState
	    (st',alerts) <- lift $ doPhysicsTick pm st
	    pushPState (st',pm)
	    modify $ \ps -> ps {psLastAlerts = alerts, psUndoneStack = ustms'}
processCommand' IMPlay (CmdManipulateToolAt pos) = do
    board <- stateBoard <$> gets psCurrentState
    wsel <- gets wrenchSelected
    void.runMaybeT $ msum $ [ do
	    tile <- liftMaybe $ snd <$> Map.lookup pos board
	    guard $ case tile of {WrenchTile _ -> True; HookTile -> True; _ -> False}
	    lift $ processCommand' IMPlay $ CmdTile tile
	] ++ [ do
	    tile <- liftMaybe $ snd <$> Map.lookup (d+^pos) board
	    guard $ tileType tile == if wsel then WrenchTile zero else HookTile
	    lift $ processCommand' IMPlay $ CmdDir WHSSelected $ neg d
	    | d <- hexDirs ]
processCommand' IMPlay (CmdDrag pos dir) = do
    board <- stateBoard <$> gets psCurrentState
    wsel <- gets wrenchSelected
    void.runMaybeT $ do
	tile <- liftMaybe $ snd <$> Map.lookup pos board
	guard $ case tile of {WrenchTile _ -> True; HookTile -> True; _ -> False}
	lift $ processCommand' IMPlay $ CmdDir WHSSelected $ dir
	board' <- stateBoard <$> gets psCurrentState
	msum [ do
		tile' <- liftMaybe $ snd <$> Map.lookup pos' board'
		guard $ tileType tile' == if wsel then WrenchTile zero else HookTile
		lift.lift $ warpPointer $ pos'
	    | pos' <- map (+^pos) $ hexDisc 2 ]

processCommand' IMPlay cmd = do
    wsel <- gets wrenchSelected
    st <- gets psCurrentState
    let push whs dir
	    | whs == WHSWrench || (whs == WHSSelected && wsel) =
		Just $ WrenchPush dir
	    | otherwise = Just $ HookPush dir
	torque whs dir
	    {- | whs == WHSHook || (whs == WHSSelected && not wsel)=
		Just $ HookTorque dir
	    | otherwise = Nothing -}
	    = Just $ HookTorque dir
	(wsel', pm) =
	    case cmd of
		CmdTile (WrenchTile _) -> (True, Nothing)
		CmdTile HookTile -> (False, Nothing)
		CmdTile (ArmTile _ _) -> (False, Nothing)
		CmdToggle -> (not wsel, Nothing)
		CmdDir whs dir -> (wsel, push whs dir)
		CmdRotate whs dir -> (wsel, torque whs dir)
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
	, rsLastAlerts = []
	, rsGameStateMoveStack = tail $ rsGameStateMoveStack rs
	, rsMoveStack = pm:rsMoveStack rs}
processCommand' IMReplay (CmdReplayBack n) = replicateM_ n $
    processCommand' IMReplay (CmdReplayBack 1)
processCommand' IMReplay (CmdReplayForward 1) = void.runMaybeT $ do
    pm <- MaybeT $ listToMaybe <$> gets rsMoveStack
    lift $ do
	st <- gets rsCurrentState
	(st',alerts) <- lift $ doPhysicsTick pm st
	modify $ \rs -> rs {rsCurrentState = st'
	    , rsLastAlerts = alerts
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
processCommand' IMEdit CmdTest = do
    frame <- gets esFrame
    modifyEState (\st -> snd $ canonify (frame, st))
    modify $ \es -> es {selectedPiece = Nothing}
    mpath <- gets esPath
    st <- gets $ head.esGameStateStack
    void.runMaybeT $ do
	soln <- solveLock (frame,st) $ Just $ "testing " ++ fromMaybe "[unnamed lock]" mpath
	lift $ modify $ \es -> es { esTested = Just (st, soln) }
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
	Nothing -> modify $ \es -> es {selectedPos = checkEditable frame selPos $ dir +^ selPos}
	Just p -> doForce $ Push p dir
processCommand' IMEdit (CmdMoveTo newPos) =
    setSelectedPos newPos
processCommand' IMEdit (CmdDrag pos dir) = do
    board <- stateBoard.head <$> gets esGameStateStack
    void.runMaybeT $ do
	selIdx <- MaybeT $ gets selectedPiece
	idx <- liftMaybe $ fst <$> Map.lookup pos board
	guard $ idx == selIdx
	lift $ processCommand' IMEdit $ CmdDir WHSSelected $ dir
	board' <- stateBoard.head <$> gets esGameStateStack
	msum [ do
		idx' <- liftMaybe $ fst <$> Map.lookup pos' board'
		guard $ idx' == selIdx
		lift.lift $ warpPointer $ pos'
	    | pos' <- [dir+^pos, pos] ]
processCommand' IMEdit (CmdRotate _ dir) = do
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
    let getDir = do
	cmd <- lift $ head <$> getSomeInput IMEdit
	case cmd of
	    CmdDir _ mergeDir -> return $ Just mergeDir
	    CmdDrag _ mergeDir -> return $ Just mergeDir
	    CmdMoveTo _ -> getDir
	    _ -> return $ Nothing
    mergeDir <- getDir
    case mergeDir of
	Just mergeDir -> modifyEState $ mergeTiles selPos mergeDir True
	_ -> return ()
    -- XXX: merging might invalidate selectedPiece
    modify $ \es -> es {selectedPiece = Nothing}
    lift $ drawMessage ""
processCommand' IMEdit CmdWait = do
    st:_ <- gets esGameStateStack
    (st',_) <- lift $ doPhysicsTick NullPM st
    pushEState st'

processCommand' IMEdit CmdDelete = do
    selPos <- gets selectedPos
    selPiece <- gets selectedPiece
    st:_ <- gets esGameStateStack
    case selPiece of
	Nothing -> drawTile selPos Nothing False
	Just p -> do modify $ \es -> es {selectedPiece = Nothing}
		     modifyEState $ delPiece p
processCommand' IMEdit CmdWriteState = void.runMaybeT $ do
    path <- lift $ gets esPath
    newPath <- MaybeT $ lift $ textInput "Save lock as:" 1024 False False Nothing path
    guard $ not $ null newPath
    fullPath <- liftIO $ fullLockPath newPath
    liftIO (doesFileExist fullPath `catchIO` const (return True)) >>?
	confirmOrBail $ "Really overwrite '"++fullPath++"'?"
    lift $ do
	st <- gets $ head.esGameStateStack
	frame <- gets esFrame
	msoln <- getCurTestSoln
	merr <- liftIO $ ((writeAsciiLockFile fullPath msoln $ canonify (frame, st)) >> return Nothing)
	    `catchIO` (return . Just . show)
	modify $ \es -> es {lastSavedState = Just (st,isJust msoln)}
	case merr of
	    Nothing -> modify $ \es -> es {esPath = Just newPath}
	    Just err -> lift $ drawError $ "Write failed: "++err
processCommand' _ _ = return ()

hashPassword :: String -> String -> String
-- ^ salt password
hashPassword name password = hash $ "IY" ++ name ++ password

setSelectedPosFromMouse :: UIMonad uiM => MainStateT uiM ()
setSelectedPosFromMouse = lift getUIMousePos >>= maybe (return ()) setSelectedPos

setSelectedPos :: Monad m => HexPos -> MainStateT m ()
setSelectedPos pos = do
    frame <- gets esFrame
    modify $ \es -> es {selectedPos = truncateToEditable frame pos}

subPlay :: UIMonad uiM => Lock -> MainStateT uiM ()
subPlay lock =
    pushEState =<< psCurrentState <$> (execSubMainState $ newPlayState lock Nothing False True)

solveLock,solveLockTut :: UIMonad uiM => Lock -> Maybe String -> MaybeT (MainStateT uiM) Solution
solveLock = solveLock' False
solveLockTut = solveLock' True
solveLock' isTut lock title = do
    (InteractSuccess solved, ps) <- lift $ runSubMainState $ newPlayState (reframe lock) title isTut False
    guard $ solved
    return $ reverse $ (map snd) $ psGameStateMoveStack ps

solveLockSaving :: UIMonad uiM => LockSpec -> Maybe SavedPlayState -> Bool -> Lock -> Maybe String -> MaybeT (MainStateT uiM) Solution
solveLockSaving ls msps isTut lock title = do
    (InteractSuccess solved, ps) <- lift $ runSubMainState $
	((maybe newPlayState restorePlayState) msps) (reframe lock) title isTut False
    if solved
	then do
	    unless isTut $ lift $ modify $ \ms -> ms { partialSolutions = Map.delete ls $ partialSolutions ms }
	    return $ reverse $ (map snd) $ psGameStateMoveStack ps
	else do
	    lift $ modify $ \ms -> if isTut then ms { tutProgress = (ls,Just $ savePlayState ps) }
		else ms { partialSolutions = Map.insert ls (savePlayState ps) $ partialSolutions ms }
	    mzero
