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
    return $ MetaState saddr undecls False auth names tvar Map.empty path lock

readLock :: FilePath -> IO (Maybe Lock)
readLock path = do
    fullPath <- if isAbsolute path then return path else
	    (++(pathSeparator:path)) <$> confFilePath "locks"
    readReadFile fullPath

writeMetaState (MetaState saddr undecls _ auth _ _ _ path _) = do
    confFilePath "metagame.conf" >>= flip writeFile (show (saddr, auth, path))
    confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>= flip writeFile (show undecls)

data InputMode = IMEdit | IMPlay | IMMeta | IMTextInput
    deriving (Eq, Ord, Show, Read)

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState _ _ _ _ _ _ _ -> IMPlay
    EditState _ _ _ _ _ _ _ -> IMEdit
    MetaState _ _ _ _ _ _ _ _ _ -> IMMeta

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
	    now <- liftIO getTime
	    tvar <- getRecordCachedFromCur True $ RecUserInfo name
	    modify $ \ms -> ms {userInfos = Map.insert name (tvar, now) $ userInfos ms}
	    liftIO $ atomically $ readTVar tvar

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
    RCUserInfo uinfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 name
    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
    return $ countUnaccessedBy ourUInfo name - countUnaccessedBy uinfo ourName
    where
	countUnaccessedBy (UserInfo _ locks _) name =
	    length $ filter (unaccessedBy name) $ elems locks
	unaccessedBy name Nothing = False
	unaccessedBy name (Just linfo) = not $ public linfo || name `elem` accessedBy linfo 

getNotesReadOn al = (maybe [] id <$>) $ runMaybeT $ do
    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
    return [ note | note <- notesRead ourUInfo, noteOn note == al ]

interactUI :: UIMonad uiM => StateT MainState uiM ()
interactUI = do
    (IMMeta==) . ms2im <$> get >>= flip when (void $ do
	flag <- gets newAsync
	unblock <- lift unblockInput
	liftIO $ forkIO $ forever $ do
	    atomically $ readTVar flag >>= check >> writeTVar flag False
	    unblock
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
	    newCodename <- liftM (map toUpper) $ MaybeT $ lift $ textInput "Codename:" 3 True Nothing
	    guard $ length newCodename == 3
	    lift $ do
		modify $ \ms -> ms { codenameStack = (newCodename:codenameStack ms) }
	processCommand' IMMeta CmdRegister = void.runMaybeT $ do
	    guard =<< isNothing <$> lift (gets curAuth)
	    newName <- MaybeT $ listToMaybe <$> gets codenameStack
	    (guard =<<) $ lift.lift $ confirm $ "Register codename "++newName++"?"
	    passwd <- MaybeT $ lift $ textInput "Enter new password:" 64 False Nothing
	    lift $ do
		modify $ \ms -> ms {curAuth = Just $ Auth newName passwd}
		resp <- curServerAction $ Register
		case resp of
		    ServerAck -> (lift $ drawMessage "Registered!")
		    ServerError err -> do
			lift $ drawMessage err
			modify $ \ms -> ms {curAuth = Nothing}
		    _ -> lift $ drawMessage "BUG"
	processCommand' IMMeta CmdAuth = void.runMaybeT $ do
	    auth <- lift $ gets curAuth
	    name <- MaybeT $ listToMaybe <$> gets codenameStack
	    passwd <- MaybeT $ lift $ textInput ("Enter password for "++name++":") 64 False Nothing
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
	    index <- MaybeT $ lift $ (charLockIndex.head <$>) <$>
		textInput "Solve which lock [A-C]?" 1 True Nothing
	    guard $ 0 <= index && index <= 2
	    ls <- MaybeT $ return $ lockSpec <$> userLocks uinfo ! index
	    tvar <- lift $ getRecordCachedFromCur False $ RecLock ls
	    RCLock lock <- MaybeT $ liftIO $ atomically $ do
		FetchedRecord fresh err rc <- readTVar tvar
		check fresh
		return rc
	    (solved,stack) <- (liftA2 (,) psSolved psGameStateMoveStack) <$>
		(lift.lift $ execStateT interactUI $ newPlayState $ reframe lock)
	    when solved $ do
		lift (gets undeclareds) >>= guard.not.or.map (\(Undeclared _ ls' _) -> ls == ls')
		modify $ \ms -> ms { undeclareds = 
		    (Undeclared (map snd stack) ls (ActiveLock name index) : undeclareds ms) }
		lift $ lift (confirm "Declare solution?") >>= flip when (processCommand' IMMeta CmdDeclare)
	processCommand' IMMeta CmdDeclare = void.runMaybeT $ do
	    name <- MaybeT $ listToMaybe <$> gets codenameStack
	    ourName <- MaybeT $ (authUser <$>) <$> gets curAuth
	    RCUserInfo ourUInfo <- MaybeT $ fetchedRC <$> getUInfoFetched 300 ourName
	    let pertinent = if name == ourName then
		    const True
		    else \(Undeclared _ _ (ActiveLock name' _)) -> name==name'
	    undecls <- lift $ filter pertinent <$> gets undeclareds
	    guard $ not $ null undecls
	    undecl@(Undeclared soln ls al) <- if length undecls == 1
		then return $ head undecls
		else do
		    which <- MaybeT $ lift $ textInput
			--("Declare which solution [1-" ++ show (length undecls) ++ "?")
			("Declare which solution?")
			5 False Nothing
		    MaybeT $ return $ msum
			[ do
			    i <- fst <$> (listToMaybe $ (reads :: ReadS Int) which)
			    guard $ 0 < i && i <= length undecls
			    return $ undecls !! i
			, do
			    listToMaybe $ [ undecl
				| undecl@(Undeclared _ _ (ActiveLock name' i)) <- undecls
				, or [ take (length which) (name' ++ ":" ++ [lockIndexChar i]) ==
					map toUpper which 
				    , name'==name && [lockIndexChar i] == which
				    ] ]
			]
	    index <- MaybeT $ lift $ (charLockIndex.head <$>) <$>
		textInput "Secure behind which lock [A-C]?" 1 True Nothing
	    guard $ 0 <= index && index <= 2
	    guard $ isJust $ userLocks ourUInfo ! index
	    lift $ do
		resp <- curServerAction $ DeclareSolution soln ls al index
		case resp of
		    ServerAck -> modify $ \ms -> ms {undeclareds = delete undecl $ undeclareds ms}
		    ServerError err -> lift $ drawError err
		    _ -> return ()
	processCommand' IMMeta CmdSelectLock = void.runMaybeT $ do
	    lockdir <- liftIO $ confFilePath "locks"
	    paths <- liftIO $ map (drop (length lockdir + 1)) <$> getDirContentsRec lockdir
	    path <- MaybeT $ lift $ textInput "Select lock: " 1024 False (Just paths)
	    lift $ do
		lock <- liftIO $ readLock path
		modify $ \ms -> ms {curLockPath = path, curLock = lock}
	    
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
	processCommand' IMEdit CmdWriteState = writeState
	    where writeState = do
		    path <- gets esPath
		    case path of
			Nothing -> do
			    newPath <- lift $ textInput "Save lock as:" 16 False Nothing
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
	textInput :: UIMonad uiM => String -> Int -> Bool -> Maybe [String] -> uiM (Maybe String)
	textInput prompt maxlen endOnMax mposss = do
	    getText ("","") <* drawMessage ""
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
