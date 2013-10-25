module Main where

import Network.Fancy

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Maybe
import Data.Word
import System.IO
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import Data.Array
import Control.Exception
import System.IO.Error

import Protocol
import Metagame
import Lock
import Frame
import Database

port = 27001 -- 27001 == ('i'<<8) + 'y'
main = do 
    setDefaultServerInfo
    writeFile lockFilePath ""
    streamServer serverSpec{address = IPv4 "" port, threading=Inline} handler
    sleepForever

setDefaultServerInfo = do
    alreadySet <- recordExists RecServerInfo
    when (not alreadySet) $ putRecord RecServerInfo (RCServerInfo defaultServerInfo)

logit = putStrLn

handler :: Handle -> Address -> IO ()
handler handle addr = do
    -- FIXME: malformed requests crash server!
    request <- B.decode `liftM` L.hGetContents handle

    let hostname = case addr of
	    IP n _ -> n
	    IPv4 n _ -> n
	    IPv6 n _ -> n
	    Unix path -> path

    hashedHostname <- hash hostname
    logit $ hashedHostname ++ " >>> " ++ show request
    response <- handleRequest request
    -- threadDelay $ 10^6
    logit $ hashedHostname ++ " <<< " ++ show response

    L.hPut handle $ B.encode response

-- | We lock the whole database during each request, using haskell's native
-- file locking, meaning that we have at any time one writer *xor* any number
-- of readers.
withDBLock lockMode m = do
    h <- getDBLock lockMode
    ret <- m
    hClose h
    return ret
    where getDBLock lockMode =
	    catchIOError (openFile lockFilePath lockMode) (\_ -> threadDelay (50*10^3) >> getDBLock lockMode)
lockFilePath = dbpath ++ "lockfile"

handleRequest :: ClientRequest -> IO ServerResponse
handleRequest req@(ClientRequest pv auth action) = do
    let lockMode = case action of
	    Authenticate -> ReadMode
	    GetServerInfo -> ReadMode
	    GetLock _ -> ReadMode
	    GetUserInfo _ _ -> ReadMode
	    _ -> ReadWriteMode
    eresp <- withDBLock lockMode $ runErrorT handleRequest'
    case eresp of
	Left error -> return $ ServerError error
	Right resp -> return resp
    where
	handleRequest' = do
	    when (pv /= protocolVersion) $ throwError "Bad protocol version"
	    case action of
		Authenticate -> checkAuth auth >> return ServerAck
		Register -> newUser auth >> return ServerAck
		GetServerInfo -> liftM ServedServerInfo $ getServerInfo
		GetLock ls -> liftM ServedLock $ getLock ls
		GetUserInfo name version -> (do
			RCUserInfo (curV,info) <- erroredIO $ getRecord $ RecUserInfo name
			if version == Just curV
			    then return ServerFresh
			    else return $ ServedUserInfo (curV,info)
		    ) `catchError` \_ -> return ServerCodenameFree
		GetSolution note -> do 
		    uinfo <- getUserInfoOfAuth auth
		    lockinfo <- getALock $ noteOn note
		    if codename uinfo == lockOwner (noteOn note) || 
			3 <= length (filter (\n -> isNothing (noteBehind n)
			    || n `elem` notesRead uinfo) $ lockSolutions lockinfo)
			then liftM ServedSolution $ getSolution note
			else throwError "You are not permitted to read that note."
		DeclareSolution soln ls target idx -> do
		    info <- getUserInfoOfAuth auth
		    lock <- getLock ls
		    tinfo <- getALock target
		    when (ls /= lockSpec tinfo) $ throwError "Lock no longer in use!"
		    when (public tinfo) $ throwError "Lock solution already public knowledge!"
		    let name = codename info
		    let behind = ActiveLock name idx
		    let note = NoteInfo name (Just behind) target
		    when (name `elem` map noteAuthor (lockSolutions tinfo)) $
			throwError "Note already taken on that lock!"
		    when (name == lockOwner target) $
			throwError "That's your lock!"
		    behindLock <- getALock behind
		    when (public behindLock) $ throwError "Your lock is cracked!"
		    when (not $ checkSolution lock soln) $ throwError "Bad solution"
		    erroredIO $ putRecord (RecNote note) (RCSolution soln)
		    execStateT (declareNote note behind) [] >>= applyDeltasToRecords
		    return ServerAck
		SetLock lock@(frame,_) idx soln -> do
		    info <- getUserInfoOfAuth auth
		    let name = codename info
		    let al = ActiveLock name idx
		    ServerInfo serverSize _ <- getServerInfo
		    when (frame /= BasicFrame serverSize) $ throwError $
			"Server only accepts size "++show serverSize++" locks."
		    when (not $ checkSolution lock soln) $ throwError "Bad solution"
		    -- TODO: check doesn't hash to existing lock
		    ls <- erroredIO $ newLockRecord lock
		    execStateT (do
			    when (isJust $ userLocks info ! idx) $
				lift (getALock al) >>= retireLock
			    addDelta name $ PutLock ls idx
			) [] >>= applyDeltasToRecords
		    return ServerAck
		_ -> throwError "BUG: bad request"
	erroredIO :: IO a -> ErrorT String IO a
	erroredIO c = do
	    ret <- liftIO $ catchIOError (liftM Right c) (return.Left)
	    case ret of
		Left e -> throwError $ "Server IO error: " ++ show e
		Right x -> return x
	getLock ls = erroredIO $ do 
	    RCLock lock <- getRecord $ RecLock ls
	    return lock
	getSolution note = erroredIO $ do 
	    RCSolution soln <- getRecord $ RecNote note
	    return soln
	getServerInfo = erroredIO $ do
	    RCServerInfo sinfo <- getRecord $ RecServerInfo
	    return sinfo
	getALock (ActiveLock name idx) = do
	    info <- getUserInfo name
	    checkValidLockIndex idx
	    case ((!idx).userLocks) info of
		Nothing -> throwError "Lock not set"
		Just lockinfo -> return lockinfo
	checkValidLockIndex idx =
	    when (not $ 0<=idx && idx < maxLocks) $ throwError "Bad lock index"
	getUserInfo name = erroredIO $ do 
	    RCUserInfo (version,info) <- getRecord $ RecUserInfo name
	    return info
	getUserInfoOfAuth auth = do 
	    checkAuth auth
	    let Just (Auth name _) = auth
	    getUserInfo name
	checkAuth :: Maybe Auth -> ErrorT String IO ()
	checkAuth Nothing = throwError "Authentication required"
	checkAuth (Just (Auth name pw)) = do
	    when (not $ validCodeName name) $ throwError "Invalid codename"
	    let rec = RecPassword name
	    hpw <- liftIO $ hash pw
	    exists <- liftIO $ recordExists rec
	    when (not exists) $ throwError "No such user"
	    RCPassword hpw' <- liftIO $ getRecord rec
	    when (hpw /= hpw') $ throwError "Wrong password"
	newUser :: Maybe Auth -> ErrorT String IO ()
	newUser Nothing = throwError "Require authentication"
	newUser (Just (Auth name pw)) = do
	    when (not $ validCodeName name) $ throwError "Invalid codename"
	    let rec = RecPassword name
	    hpw <- liftIO $ hash pw
	    exists <- liftIO $ recordExists rec
	    when exists $ throwError "Codename taken"
	    erroredIO $ putRecord rec (RCPassword hpw)
	    erroredIO $ putRecord (RecUserInfo name) (RCUserInfo $ (1,initUserInfo name))
	--- | TODO: journalling so we can survive death during database writes?
	applyDeltasToRecords :: [(Codename, UserInfoDelta)] -> ErrorT String IO ()
	applyDeltasToRecords = (mapM_ applyDeltaToRecords) . reverse
	applyDeltaToRecords (name,delta) =
	    erroredIO $ modifyRecord (RecUserInfo name) $
		\(RCUserInfo (v,info)) -> RCUserInfo $ (v+1,applyDelta delta info)
	declareNote note@(NoteInfo _ _ target) behind@(ActiveLock name idx) = do
	    accessLock name target =<< getCurrALock target
	    addDelta (lockOwner target) $ LockDelta (lockIndex target) $ AddSolution note
	    addDelta name $ LockDelta idx $ AddSecured note
	    accessed <- accessedBy `liftM` getCurrALock behind
	    mapM_ (addReadNote note) (name:accessed)
	addReadNote note@(NoteInfo _ _ target) name = do
	    info <- getCurrUserInfo name
	    tlock <- getCurrALock target
	    when (not $ name `elem` accessedBy tlock || note `elem` notesRead info) $ do
		addDelta name $ AddRead note
		checkSuffReadNotes target name
	accessLock name target@(ActiveLock tname ti) tlock = do
	    addDelta tname $ LockDelta ti $ AddAccessed name
	    mapM_ (flip addReadNote name) $ notesSecured tlock
	publiciseLock al@(ActiveLock name idx) lock = do
	    addDelta name $ LockDelta idx SetPublic
	    retireLock lock
	retireLock lock = do
	    mapM_ scrapNote $ lockSolutions lock
	    mapM_ publiciseNote $ notesSecured lock
	scrapNote note@(NoteInfo _ (Just al@(ActiveLock name idx)) _) = do
	    addDelta name $ LockDelta idx (DelSecured note)
	    unreadNote note
	scrapNote _ = return ()
	unreadNote note@(NoteInfo name (Just al) _) = do
	    lock <- getCurrALock al
	    mapM_ (\name' -> addDelta name' (DelRead note)) $ name:(accessedBy lock)
	publiciseNote note@(NoteInfo _ _ al@(ActiveLock name idx)) = do
	    unreadNote note
	    addDelta name $ LockDelta idx $ SetPubNote note
	    publified <- checkSuffPubNotes al
	    when (not publified) $ do
	    lock <- getCurrALock al
	    accessorsOfNotesOnLock <- ((++ map noteAuthor (lockSolutions lock)).concat)
		`liftM` (sequence 
		    [ accessedBy `liftM` getCurrALock behind | NoteInfo _ (Just behind) _ <- lockSolutions lock ] )
	    flip mapM_ accessorsOfNotesOnLock $ checkSuffReadNotes al
	checkSuffReadNotes target name = do
	    info <- getCurrUserInfo name
	    tlock <- getCurrALock target
	    let countRead = fromIntegral $ length $
		    filter (\n -> isNothing (noteBehind n) || n `elem` notesRead info) $ lockSolutions tlock
	    when (not (public tlock) && countRead == notesNeeded) $
		accessLock name target tlock
	checkSuffPubNotes al@(ActiveLock name idx) = do
	    info <- getCurrUserInfo name
	    let Just lock = userLocks info ! idx
	    let countPub = fromIntegral $ length $
		    filter (isNothing.noteBehind) $ lockSolutions lock
	    if (countPub == notesNeeded)
		then publiciseLock al lock >> return True
		else return False
	-- | XXX we apply deltas right-to-left, so in the order of adding
	addDelta name delta = modify ((name,delta):)
	getCurrUserInfo name = do
	    info <- lift $ getUserInfo name
	    get >>= return . foldr applyDelta info . map snd . filter (\(n,_) -> n == name)
	getCurrALock al@(ActiveLock name idx) = do
	    getCurrUserInfo name >>= return.fromJust.(!idx).userLocks
