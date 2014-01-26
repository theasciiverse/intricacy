-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Main where

import Network.Fancy

import Control.Concurrent (threadDelay)
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans
import Control.Monad.IO.Class
import System.Random
import Data.Maybe
import Data.Word
import Data.Foldable (for_)
import Data.Time.Clock
import System.IO
import System.FilePath
import qualified Data.ByteString.Lazy as L
import qualified Data.Binary as B
import Data.Array
import Control.Exception
import System.IO.Error
import Data.List
import Data.Function (on)
import Pipes
import qualified Pipes.Prelude as P

import System.Environment
import System.Console.GetOpt

import Protocol
import Metagame
import Lock
import Frame
import Database
import AsciiLock
import Mundanities

port = 27001 -- 27001 == ('i'<<8) + 'y'

data Opt = RequestDelay Int
    deriving (Eq, Ord, Show)
options =
    [ Option ['d'] ["delay"] (ReqArg (RequestDelay . read) "MICROSECS") "delay before sending response (for testing) (default: 0)"
    --, Option ['d'] ["dir"] (ReqArg (DBDir . read)) "directory for server database [default: intricacydb]"
    ]
parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
	(o,n,[]) -> return (o,n)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: intricacy-server [OPTION...]"

main = do 
    argv <- getArgs
    (opts,_) <- parseArgs argv
    let delay = fromMaybe 0 $ listToMaybe [ d | RequestDelay d <- opts ]
    withDB setDefaultServerInfo
    writeFile lockFilePath ""
    streamServer serverSpec{address = IPv4 "" port, threading=Inline} $ handler delay
    sleepForever

-- database path is relative to current directory: ./intricacydb/
dbpath = "intricacydb"
withDB = flip runReaderT dbpath

setDefaultServerInfo = do
    alreadySet <- recordExists RecServerInfo
    unless alreadySet $ putRecord RecServerInfo (RCServerInfo defaultServerInfo)

logit = putStrLn

handler :: Int -> Handle -> Address -> IO ()
handler delay hdl addr = handle ((\e -> return ()) :: SomeException -> IO ()) $
    handler' hdl addr
    where handler' hdl addr = do
	    response <- handle (\e -> return $ ServerError $ show (e::SomeException)) $ do
		request <- B.decode <$> L.hGetContents hdl 
		let hostname = case addr of
			IP n _ -> n
			IPv4 n _ -> n
			IPv6 n _ -> n
			Unix path -> path
		    hashedHostname = take 8 $ hash hostname
		now <- liftIO getCurrentTime
		logit $ show now ++ ": " ++ hashedHostname ++ " >>> " ++ showRequest request
		response <- handleRequest request
		when (delay > 0) $ threadDelay delay
		now' <- liftIO getCurrentTime
		logit $ show now' ++ ": " ++ hashedHostname ++ " <<< " ++ showResponse response
		return response
	    L.hPut hdl $ B.encode response

showRequest :: ClientRequest -> String
showRequest (ClientRequest ver mauth act) = show ver ++ " "
    ++ maybe "" (\(Auth name pw) -> "Auth:" ++ name ++ ":" ++ take 4 pw) mauth ++ " "
    ++ showAction act
showAction :: Action -> String
showAction (SetLock lock idx soln) = "SetLock " ++ show idx ++ " lock:"
    ++ (if not $ validLock $ reframe lock then " [INVALID LOCK] " else "\n" ++ unlines (lockToAscii lock))
    ++ "[SOLN]"
showAction (DeclareSolution soln ls target idx) = "DeclareSolution [SOLN] "
    ++ (concat $ intersperse " " $ [show ls,show target,show idx])
showAction act = show act
showResponse :: ServerResponse -> String
showResponse (ServedLock lock) = "ServedLock lock:\n" ++ unlines (lockToAscii lock)
showResponse (ServedSolution soln) = "ServedSolution [SOLN]"
showResponse resp = show resp

-- | We lock the whole database during each request, using haskell's native
-- file locking, meaning that we have at any time one writer *xor* any number
-- of readers.
withDBLock lockMode m = do
    h <- getDBLock lockMode
    ret <- m
    hClose h
    return ret
    where getDBLock lockMode =
	    catchIO (openFile lockFilePath lockMode) (\_ -> threadDelay (50*10^3) >> getDBLock lockMode)
lockFilePath = dbpath ++ [pathSeparator] ++ "lockfile"

handleRequest :: ClientRequest -> IO ServerResponse
handleRequest req@(ClientRequest pv auth action) = do
    let lockMode = case action of
	    Authenticate -> ReadMode
	    GetServerInfo -> ReadMode
	    GetLock _ -> ReadMode
	    GetUserInfo _ _ -> ReadMode
	    GetRetired _ -> ReadMode
	    GetSolution _ -> ReadMode
	    GetRandomNames _ -> ReadMode
	    _ -> ReadWriteMode
    eresp <- withDBLock lockMode $ runErrorT handleRequest'
    case eresp of
	Left error -> return $ ServerError error
	Right resp -> return resp
    where
	handleRequest' = do
	    when (pv /= protocolVersion) $ throwError "Bad protocol version"
	    case action of
		Authenticate -> do 
		    checkAuth auth 
		    return $ ServerMessage $ "Welcome, " ++ authUser (fromJust auth)
		Register -> newUser auth >> return ServerAck
		ResetPassword passwd -> resetPassword auth passwd >> return ServerAck
		GetServerInfo -> ServedServerInfo <$> getServerInfo
		GetLock ls -> ServedLock <$> getLock ls
		GetRetired name -> ServedRetired <$> getRetired name
		GetUserInfo name mversion -> (do
			RCUserInfo (curV,info) <- getRecordErrored $ RecUserInfo name
			(fromJust<$>)$ runMaybeT $ msum [ do
			    v <- MaybeT $ return mversion
			    msum [ guard (v >= curV) >> return ServerFresh
				, do 
				    guard (v >= curV - 10) 
				    RCUserInfoDeltas deltas <- lift $ getRecordErrored $ RecUserInfoLog name
				    return $ ServedUserInfoDeltas $ take (curV-v) deltas
				]
			    , return $ ServedUserInfo (curV,info)
			    ]
		    ) `catchError` \_ -> return ServerCodenameFree
		GetSolution note -> do 
		    uinfo <- getUserInfoOfAuth auth
		    lockinfo <- getALock $ noteOn note
		    if codename uinfo == lockOwner (noteOn note)
			|| codename uinfo == noteAuthor note 
			|| 3 <= length (filter (\n -> isNothing (noteBehind n)
			    || n `elem` notesRead uinfo) $ lockSolutions lockinfo)
			then ServedSolution <$> getSolution note
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
		    unless (checkSolution lock soln) $ throwError "Bad solution"
		    erroredDB $ putRecord (RecNote note) (RCSolution soln)
		    execStateT (declareNote note behind) [] >>= applyDeltasToRecords
		    return ServerAck
		SetLock lock@(frame,_) idx soln -> do
		    info <- getUserInfoOfAuth auth
		    let name = codename info
		    let al = ActiveLock name idx
		    ServerInfo serverSize _ <- getServerInfo
		    when (frame /= BasicFrame serverSize) $ throwError $
			"Server only accepts size "++show serverSize++" locks."
		    unless (validLock $ reframe lock) $ throwError "Invalid lock!"
		    unless (checkSolution lock soln) $ throwError "Bad solution"

		    RCLockHashes hashes <- getRecordErrored RecLockHashes
			    `catchError` const (return (RCLockHashes []))
		    let hashed = hash $ show lock
		    when (hashed `elem` hashes) $ throwError "Lock has already been used"
		    ls <- erroredDB $ newLockRecord lock
		    erroredDB $ putRecord RecLockHashes $ RCLockHashes $ hashed:hashes

		    let oldLockInfo = userLocks info ! idx
		    execStateT (do
			    when (isJust $ oldLockInfo) $
				lift (getALock al) >>= retireLock
			    addDelta name $ PutLock ls idx
			) [] >>= applyDeltasToRecords

		    for_ oldLockInfo $ \oldui -> do
			lss <- getRetired name
			erroredDB $ putRecord (RecRetiredLocks name) $ RCLockSpecs $ (lockSpec oldui):lss
		    return ServerAck
		GetRandomNames n -> do
		    names <- erroredDB $ listUsers
		    gen <- erroredIO $ getStdGen
		    let l = length names
			namesArray = listArray (0,l-1) names
			negligible name = do
			    uinfo <- getUserInfo name
			    return $ all (maybe True public . (userLocks uinfo !)) [0..2]

		    -- huzzah for pipes!
		    shuffled <- P.toListM $
			    mapM_ Pipes.yield (nub $ randomRs (0,l-1) gen)
			>-> P.take l -- give up once we've permuted all of [0..l-1]
			>-> P.map (namesArray !)
			>-> P.filterM ((not <$>) . negligible) -- throw away negligibles
			>-> P.take n -- try to take as many as we were asked for
		    liftIO newStdGen
		    return $ ServedRandomNames shuffled
		_ -> throwError "BUG: bad request"
	erroredIO :: IO a -> ErrorT String IO a
	erroredIO c = do
	    ret <- liftIO $ catchIO (Right <$> c) (return.Left)
	    case ret of
		Left e -> throwError $ "Server IO error: " ++ show e
		Right x -> return x
	erroredDB :: DBM a -> ErrorT String IO a
	erroredDB = erroredIO . withDB
	getRecordErrored :: Record -> ErrorT String IO RecordContents
	getRecordErrored rec = do
	    mrc <- lift $ withDB $ getRecord rec 
	    case mrc of
		Just rc -> return rc
		Nothing -> throwError $ "Bad record on server! Record was: " ++ show rec
	getLock ls = do 
	    RCLock lock <- getRecordErrored $ RecLock ls
	    return lock
	getSolution note = do 
	    RCSolution soln <- getRecordErrored $ RecNote note
	    return soln
	getServerInfo = do
	    RCServerInfo sinfo <- getRecordErrored $ RecServerInfo
	    return sinfo
	getRetired name = do
	    RCLockSpecs lss <- fromMaybe (RCLockSpecs []) <$> (erroredDB $ getRecord $ RecRetiredLocks name)
	    return lss
	getALock (ActiveLock name idx) = do
	    info <- getUserInfo name
	    checkValidLockIndex idx
	    case ((!idx).userLocks) info of
		Nothing -> throwError "Lock not set"
		Just lockinfo -> return lockinfo
	checkValidLockIndex idx =
	    unless (0<=idx && idx < maxLocks) $ throwError "Bad lock index"
	getUserInfo name = do 
	    RCUserInfo (version,info) <- getRecordErrored $ RecUserInfo name
	    return info
	getUserInfoOfAuth auth = do 
	    checkAuth auth
	    let Just (Auth name _) = auth
	    getUserInfo name
	    
	checkAuth :: Maybe Auth -> ErrorT String IO ()
	checkAuth Nothing = throwError "Authentication required"
	checkAuth (Just (Auth name pw)) = do
	    exists <- checkCodeName name
	    unless exists $ throwError "No such user"
	    RCPassword pw' <- getRecordErrored (RecPassword name)
	    when (pw /= pw') $ throwError "Wrong password"
	newUser :: Maybe Auth -> ErrorT String IO ()
	newUser Nothing = throwError "Require authentication"
	newUser (Just (Auth name pw)) = do
	    exists <- checkCodeName name
	    when exists $ throwError "Codename taken"
	    erroredDB $ putRecord (RecPassword name) (RCPassword pw)
	    erroredDB $ putRecord (RecUserInfo name) (RCUserInfo $ (1,initUserInfo name))
	    erroredDB $ putRecord (RecUserInfoLog name) (RCUserInfoDeltas [])
	resetPassword Nothing pw = throwError "Authentication required"
	resetPassword auth@(Just (Auth name _)) newpw = do
	    checkAuth auth
	    erroredDB $ putRecord (RecPassword name) (RCPassword newpw)
	checkCodeName :: Codename -> ErrorT String IO Bool
	checkCodeName name = do
	    unless (validCodeName name) $ throwError "Invalid codename"
	    liftIO $ withDB $ recordExists $ RecPassword name
	--- | TODO: journalling so we can survive death during database writes?
	applyDeltasToRecords :: [(Codename, UserInfoDelta)] -> ErrorT String IO ()
	applyDeltasToRecords nds = sequence_ $ [applyDeltasToRecord name deltas
		| group <- groupBy ((==) `on` fst) nds 
		, let name = fst $ head group
		, let deltas = map snd group ]
	applyDeltasToRecord name deltas = do
	    erroredDB $ modifyRecord (RecUserInfoLog name) $
		\(RCUserInfoDeltas deltas') -> RCUserInfoDeltas $ deltas ++ deltas'
	    erroredDB $ modifyRecord (RecUserInfo name) $
		\(RCUserInfo (v,info)) -> RCUserInfo $
		    (v+length deltas, applyDeltas info deltas)
	declareNote note@(NoteInfo _ _ target) behind@(ActiveLock name idx) = do
	    accessLock name target =<< getCurrALock target
	    addDelta (lockOwner target) $ LockDelta (lockIndex target) $ AddSolution note
	    addDelta name $ LockDelta idx $ AddSecured note
	    accessed <- accessedBy <$> getCurrALock behind
	    mapM_ (addReadNote note) (name:accessed)
	addReadNote note@(NoteInfo _ _ target) name = do
	    info <- getCurrUserInfo name
	    tlock <- getCurrALock target
	    unless (name `elem` accessedBy tlock || note `elem` notesRead info) $ do
		addDelta name $ AddRead note
		checkSuffReadNotes target name
	accessLock name target@(ActiveLock tname ti) tlock = do
	    addDelta tname $ LockDelta ti $ AddAccessed name
	    mapM_ (`addReadNote` name) $ notesSecured tlock
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
	    unless publified $ do
	    lock <- getCurrALock al
	    accessorsOfNotesOnLock <- ((++ map noteAuthor (lockSolutions lock)).concat)
		<$> (sequence 
		    [ accessedBy <$> getCurrALock behind | NoteInfo _ (Just behind) _ <- lockSolutions lock ] )
	    forM_ accessorsOfNotesOnLock $ checkSuffReadNotes al
	checkSuffReadNotes target name = do
	    info <- getCurrUserInfo name
	    tlock <- getCurrALock target
	    let countRead = fromIntegral $ length $
		    filter (\n -> isNothing (noteBehind n) || n `elem` notesRead info) $ lockSolutions tlock
	    when (countRead == notesNeeded && not (public tlock) && name /= lockOwner target) $
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
	    (applyDeltas info . map snd . filter ((==name).fst)) <$> get
	getCurrALock al@(ActiveLock name idx) =
	     (fromJust.(!idx).userLocks) <$> getCurrUserInfo name
