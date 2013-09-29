module Main where

--import Data.BinaryCom as BC
import Network.Fancy

import System.IO
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans.Error
import Control.Monad.Trans
import Control.Monad.IO.Class
import Data.Maybe
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Binary as B
import Data.Array
import Control.Exception
import System.IO.Error
import OpenSSL.Digest (MessageDigest(SHA1))
import OpenSSL.Digest.ByteString.Lazy (digest)

import Protocol
import Metagame
import Lock
import Frame
import GameState

port = 27001 -- 27001 == ('i'<<8) + 'y'
main = do 
    setDefaultServerInfo
    streamServer serverSpec{address = IPv4 "" port} handler
    sleepForever

setDefaultServerInfo = do
    alreadySet <- recordExists RecServerInfo
    when (not alreadySet) $ putRecord RecServerInfo (RCServerInfo defaultServerInfo)

hash :: String -> IO String
hash str = fmap (>>=toHex) $ digest SHA1 $ CL.pack $ str

toHex :: Word8 -> String
toHex n = let (n1,n2) = divMod (fromIntegral n) 16
	in map toHex' [n1,n2]
    where toHex' n | n < 10 = chr $ ord '0' + n 
	    | n < 16 = chr $ ord 'a' + n - 10

logit = putStrLn

handler :: Handle -> Address -> IO ()
handler handle addr = do
    --com <- BC.binaryCom handle
    --request <- BC.receive com

    --request <- read `liftM` hGetContents handle

    request <- B.decode `liftM` L.hGetContents handle

    let hostname = case addr of
	    IP n _ -> n
	    IPv4 n _ -> n
	    IPv6 n _ -> n
	    Unix path -> path

    hashedHostname <- hash hostname
    logit $ hashedHostname ++ " >>> " ++ show request
    response <- handleRequest request
    logit $ hashedHostname ++ " <<< " ++ show response

    L.hPut handle $ B.encode response

    --hPutStr handle $ show response

    --BC.send com response

handleRequest :: ClientRequest -> IO ServerResponse
handleRequest req@(ClientRequest pv auth action) = do
    eresp <- runErrorT handleRequest'
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
		GetUserInfo name -> liftM ServedUserInfo $ getUserInfo name
		DeclareSolution soln ls target idx -> do
		    info <- getUserInfoOfAuth auth
		    lock <- getLock ls
		    when (not $ checkSolution lock soln) $ throwError "Bad solution"
		    tinfo <- getALock target
		    when (ls /= lockSpec tinfo) $ throwError "Lock no longer in use!"
		    let name = codename info
		    let note = NoteInfo name target
		    let behind = ActiveLock name idx
		    erroredIO $ putRecord (RecNote note) (RCSolution soln)
		    execStateT (stowNote note behind) [] >>= applyDeltasToRecords
		    return ServerAck
		SetLock lock@(frame,_) idx soln -> do
		    info <- getUserInfoOfAuth auth
		    let name = codename info
		    let al = ActiveLock name idx
		    ServerInfo serverSize _ <- getServerInfo
		    when (frame /= BasicFrame serverSize) $ throwError "Bad frame"
		    when (not $ checkSolution lock soln) $ throwError "Bad solution"
		    ls <- erroredIO $ newLockRecord lock
		    execStateT (do
			when (isJust $ userLocks info ! idx) $ retireLock al
			prependDelta name $ PutLock ls idx) [] >>= applyDeltasToRecords
		    return ServerAck
		_ -> throwError "BUG: bad request"
	erroredIO :: IO a -> ErrorT String IO a
	erroredIO c = do
	    ret <- liftIO $ catchIOError (liftM Just c) (\_ -> return Nothing)
	    case ret of
		Nothing -> throwError "IO error"
		Just x -> return x
	getLock ls = erroredIO $ do 
	    RCLock lock <- getRecord $ RecLock ls
	    return lock
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
	    RCUserInfo info <- getRecord $ RecUserInfo name
	    return info
	getUserInfoOfAuth auth = do 
	    checkAuth auth
	    let Just (Auth name _) = auth
	    getUserInfo name
	checkAuth :: Maybe Auth -> ErrorT String IO ()
	checkAuth Nothing = throwError "Require authentication"
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
	    erroredIO $ putRecord (RecUserInfo name) (RCUserInfo $ initUserInfo name)
	applyDeltasToRecords :: [(Codename, UserInfoDelta)] -> ErrorT String IO ()
	applyDeltasToRecords = mapM_ applyDeltaToRecords
	applyDeltaToRecords (name,delta) =
	    erroredIO $ modifyRecord (RecUserInfo name) $
		\(RCUserInfo info) -> RCUserInfo $ applyDelta delta info
	stowNote note behind@(ActiveLock name idx) = do
	    prependDelta name $ LockDelta idx $ AddSecured note
	    prependDelta (lockOwner $ noteOn note) $ LockDelta idx $ AddSolution note
	    accessed <- lift $ accessedBy `liftM` getALock behind
	    mapM_ (addReadNote note) (name:accessed)
	addReadNote note@(NoteInfo _ target) name = do
	    info <- lift $ getUserInfo name
	    tlock <- lift $ getALock target
	    when (not $ name `elem` accessedBy tlock || note `elem` notesRead info) $ do
		prependDelta name $ AddRead note
		checkSuffReadNotes name info target tlock
	accessLock name target@(ActiveLock tname ti) tlock = do
	    prependDelta tname $ LockDelta ti $ AddAccessed name
	    mapM_ (flip addReadNote name) $ notesSecured tlock
	publiciseLock al@(ActiveLock name idx) lock = do
	    prependDelta name $ LockDelta idx SetPublic
	    mapM_ publiciseNote $ notesSecured lock
	retireLock al@(ActiveLock name idx) = do
	    lock <- lift $ getALock al
	    mapM_ publiciseNote $ notesSecured lock
	publiciseNote note@(NoteInfo _ al@(ActiveLock name idx)) = do
	    prependDelta name $ LockDelta idx $ AddPubNote note
	    lock <- lift $ getALock al
	    publified <- checkSuffPubNotes al lock
	    when (not publified) $ do
	    accessorsOfNotesOnLock <- ((++ map noteAuthor (lockSolutions lock)).concat)
		`liftM` (sequence 
		    [ accessedBy `liftM` lift (getALock (noteOn n)) | n <- lockSolutions lock ] )
	    flip mapM_ accessorsOfNotesOnLock (\name' -> do
		info' <- lift $ getUserInfo name'
		checkSuffReadNotes name' info' al lock)
	checkSuffReadNotes name info target tlock = do
	    added <- gets $ filter (\delta -> case delta of 
		    (name', AddRead (NoteInfo _ target')) -> name==name' && target==target'
		    (_, LockDelta _ (AddPubNote (NoteInfo _ target'))) -> target==target'
		    _ -> False)
	    let countRead =
		    (length $ filter (\n -> noteOn n == target) $ notesRead info) +
		    (length $ publicNotes tlock) +
		    (length added)
	    when (fromIntegral countRead == notesNeeded) $
		accessLock name target tlock
	checkSuffPubNotes al lock = do
	    added <- gets $ filter (\delta -> case delta of 
		    (_, LockDelta _ (AddPubNote (NoteInfo _ al'))) -> al==al'
		    _ -> False)
	    let countRead = (length $ publicNotes lock) + (length added)
	    if (fromIntegral countRead == notesNeeded)
		then publiciseLock al lock >> return True
		else return False
	    
	prependDelta name delta = modify ((name,delta):)

	    
	

validCodeName name = length name == 3 && and (map (validChar) name)
    where validChar c = isPrint c && not (isLower c) && c /= ' '

data Record
    = RecPassword Codename
    | RecUserInfo Codename
    | RecLock LockSpec
    | RecNote NoteInfo
    | RecServerInfo
    deriving (Eq, Ord, Show)
data RecordContents
    = RCPassword Password
    | RCUserInfo UserInfo
    | RCLock Lock
    | RCSolution Solution
    | RCServerInfo ServerInfo
    deriving (Eq, Ord, Show)

recordExists :: Record -> IO Bool
recordExists = doesFileExist . recordPath

getRecord :: Record -> IO RecordContents
getRecord rec = getRecord' rec
    where
	path = recordPath rec
	getRecord' (RecPassword _) = liftM RCPassword $ read `liftM` readFile path
	getRecord' (RecUserInfo _) = liftM RCUserInfo $ read `liftM` readFile path
	getRecord' (RecLock _) = liftM RCLock $ read `liftM` readFile path
	getRecord' (RecNote _) = liftM RCSolution $ read `liftM` readFile path
	getRecord' (RecServerInfo) = liftM RCServerInfo $ read `liftM` readFile path

putRecord :: Record -> RecordContents -> IO ()
putRecord rec rc = putRecord' rc
    where
	path = recordPath rec
	putFile str = mkdirhier path >> writeFile path str
	putRecord' (RCPassword hpw) = putFile $ show hpw
	putRecord' (RCUserInfo info) = putFile $ show info
	putRecord' (RCLock lock) = putFile $ show lock
	putRecord' (RCSolution solution) = putFile $ show solution
	putRecord' (RCServerInfo sinfo) = putFile $ show sinfo

-- | relying on laziness to actually open the file for writing (hence locking
-- it) before reading
modifyRecord :: Record -> (RecordContents -> RecordContents) -> IO ()
modifyRecord rec f = f `liftM` getRecord rec >>= putRecord rec

newLockRecord :: Lock -> IO LockSpec
newLockRecord lock = do
    let path = dbpath++"lastlock"
    ls <- catchIOError (((1+).read) `liftM` readFile path) (\_ -> return 0)
    writeFile path $ show ls
    putRecord (RecLock ls) (RCLock lock)
    return ls

mkdirhier path = let dir = takeDirectory path in do
    exists <- doesDirectoryExist dir
    when (not exists) $ mkdirhier dir >> createDirectory dir

recordPath :: Record -> FilePath
recordPath = (dbpath++) . recordPath'
    where
	recordPath' (RecPassword name) = userDir name ++ "passwd"
	recordPath' (RecUserInfo name) = userDir name ++ "info"
	recordPath' (RecLock ls) = locksDir ++ show ls
	recordPath' (RecNote (NoteInfo name alock)) = userDir name ++ "notes/" ++ alockFN alock
	recordPath' (RecServerInfo) = "serverInfo"

	userDir name = "users/" ++ pathifyName name ++ "/"
	pathifyName = map (\c -> case c of {'/'->'s'; '.'->'d'; '\\'->'b'; _ -> c})
	alockFN (ActiveLock name idx) = pathifyName name++":"++(show idx)
	locksDir = "locks/"
dbpath = "intricacydb/"
