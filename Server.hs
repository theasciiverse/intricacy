module Main where

--import Data.BinaryCom as BC
import Network.Fancy

import System.IO
import System.FilePath
import System.Directory
import Control.Monad
import Control.Monad.Trans.Error
import Control.Monad.IO.Class
import Data.Char
import Data.Word
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Binary as B
import Control.Exception
import System.IO.Error
import OpenSSL.Digest (MessageDigest(SHA1))
import OpenSSL.Digest.ByteString.Lazy (digest)

import Protocol
import Metagame
import Lock
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

log = print

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
    log $ hashedHostname ++ " >>> " ++ show request
    response <- handleRequest request
    log $ hashedHostname ++ " <<< " ++ show response

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
		DeclareSolution soln ls al idx -> do
		    info <- getUserInfoOfAuth auth
		    lock <- getLock ls
		    when (not $ checkSolution lock soln) $ throwError "Bad solution"
		    ls' <- getALock al
		    when (ls /= ls') $ throwError "Lock no longer in use!"
		    solSpec <- addSolution soln
		    checkValidLockIndex idx info
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
	    checkValidLockIndex idx info
	    return $ (lockSpec.(!!idx).userLocks) info
	checkValidLockIndex idx info =
	    when (not $ idx `elem` [0 .. length (userLocks info) - 1]) $
		throwError "Bad lock index"
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

validCodeName name = length name == 3 && and (map (validChar) name)
    where validChar c = isPrint c && not (isLower c) && c /= ' '

data Record
    = RecPassword Codename
    | RecUserInfo Codename
    | RecLock LockSpec
    | RecNote Codename ActiveLock
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
	getRecord' (RecNote _ _) = liftM RCSolution $ read `liftM` readFile path
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

mkdirhier path = let dir = takeDirectory path in do
    exists <- doesDirectoryExist dir
    when (not exists) $ mkdirhier dir >> createDirectory dir

recordPath :: Record -> FilePath
recordPath = (dbpath++) . recordPath'
    where
	recordPath' (RecPassword name) = userDir name ++ "passwd"
	recordPath' (RecUserInfo name) = userDir name ++ "info"
	recordPath' (RecLock ln) = locksDir ++ show ln
	recordPath' (RecNote name alock) = userDir name ++ "notes/" ++ alockFN alock
	recordPath' (RecServerInfo) = "serverInfo"

	userDir name = "users/" ++ pathifyName name ++ "/"
	pathifyName = map (\c -> case c of {'/'->'s'; '.'->'d'; '\\'->'b'; _ -> c})
	alockFN (ActiveLock name idx) = pathifyName name++":"++(show idx)
	locksDir = "locks/"
	dbpath = "intricacydb/"
