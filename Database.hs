module Database where

-- TODO: use ByteString everywhere

import Control.Applicative
import Control.Monad
import System.IO
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as CS

--import OpenSSL.Digest (MessageDigest(SHA1), toHex)
--import OpenSSL.Digest.ByteString.Lazy (digest)
--import Data.Digest.Pure.SHA (sha1, showDigest)
--see also OpenSSL.EVP.Digest from HsOpenSSL
import Crypto.Hash (hashlazy, Digest, SHA1, digestToHexByteString)

import Protocol
import Metagame
import Lock
import Mundanities

--hash :: String -> IO String
--hash str = fmap (>>=toHex) $ digest SHA1 $ CL.pack $ str
sha1 :: CL.ByteString -> Digest SHA1
sha1 = hashlazy
hash :: String -> String
hash = CS.unpack . digestToHexByteString . sha1 . CL.pack


data Record
    = RecPassword Codename
    | RecUserInfo Codename
    | RecUserInfoLog Codename
    | RecLock LockSpec
    | RecNote NoteInfo
    | RecLockHashes
    | RecServerInfo
    deriving (Eq, Ord, Show)
data RecordContents
    = RCPassword Password
    | RCUserInfo VersionedUInfo
    | RCUserInfoDeltas [UserInfoDelta]
    | RCLock Lock
    | RCSolution Solution
    | RCLockHashes [String]
    | RCServerInfo ServerInfo
    deriving (Eq, Ord, Show)

rcOfServerResp (ServedServerInfo x) = RCServerInfo x
rcOfServerResp (ServedLock x) = RCLock x
rcOfServerResp (ServedSolution x) = RCSolution x
rcOfServerResp (ServedUserInfo x) = RCUserInfo x
rcOfServerResp _ = error "no corresponding rc"

invariantRecord (RecUserInfo _) = False
invariantRecord (RecUserInfoLog _) = False
invariantRecord (RecPassword _) = False
invariantRecord _ = True

askForRecord RecServerInfo = GetServerInfo
askForRecord (RecUserInfo name) = GetUserInfo name Nothing
askForRecord (RecLock ls) = GetLock ls
askForRecord (RecNote note) = GetSolution note
askForRecord _ = error "no corresponding request"
	    
recordExists :: Record -> IO Bool
recordExists = doesFileExist . recordPath

getRecord :: Record -> IO (Maybe RecordContents)
getRecord rec = flip catchIO (const $ return Nothing) $ do
    h <- openFile (recordPath rec) ReadMode
    getRecordh rec h <* hClose h
getRecordh (RecPassword _) h = ((RCPassword <$>) . tryRead) <$> hGetStrict h
getRecordh (RecUserInfo _) h = ((RCUserInfo <$>) . tryRead) <$> hGetStrict h
getRecordh (RecUserInfoLog _) h = ((RCUserInfoDeltas <$>) . tryRead) <$> hGetStrict h
getRecordh (RecLock _) h = ((RCLock <$>) . tryRead) <$> hGetStrict h
getRecordh (RecNote _) h = ((RCSolution <$>) . tryRead) <$> hGetStrict h
getRecordh RecLockHashes h = ((RCLockHashes <$>) . tryRead) <$> hGetStrict h
getRecordh RecServerInfo h = ((RCServerInfo <$>) . tryRead) <$> hGetStrict h

hGetStrict h = CS.unpack `liftM` concatMWhileNonempty (repeat $ CS.hGet h 1024)
    where concatMWhileNonempty (m:ms) = do
	    bs <- m
	    if CS.null bs
		then return bs
		else (bs `CS.append`) `liftM` concatMWhileNonempty ms

putRecord :: Record -> RecordContents -> IO ()
putRecord rec rc = do
    let path = recordPath rec
    mkdirhierto path 
    h <- openFile path WriteMode 
    putRecordh rc h
    hClose h
putRecordh (RCPassword hpw) h = hPutStr h $ show hpw
putRecordh (RCUserInfo info) h = hPutStr h $ show info
putRecordh (RCUserInfoDeltas deltas) h = hPutStr h $ show deltas
putRecordh (RCLock lock) h = hPutStr h $ show lock
putRecordh (RCSolution solution) h = hPutStr h $ show solution
putRecordh (RCLockHashes hashes) h = hPutStr h $ show hashes
putRecordh (RCServerInfo sinfo) h = hPutStr h $ show sinfo

modifyRecord :: Record -> (RecordContents -> RecordContents) -> IO ()
modifyRecord rec f = do
    h <- openFile (recordPath rec) ReadWriteMode 
    Just rc <- getRecordh rec h
    hSeek h AbsoluteSeek 0
    putRecordh (f rc) h
    hTell h >>= hSetFileSize h 
    hClose h

delRecord :: Record -> IO ()
delRecord rec = removeFile $ recordPath rec

newLockRecord :: Lock -> IO LockSpec
newLockRecord lock = do
    let path = dbpath++"lastlock"
    h <- openFile path ReadWriteMode
    contents <- hGetStrict h
    let ls = if null contents then 0 else 1 + read contents
    hSeek h AbsoluteSeek 0
    hPutStr h $ show ls
    hClose h
    putRecord (RecLock ls) (RCLock lock)
    return ls

listUsers :: IO [Codename]
listUsers = filter ((==3).length) <$> getDirectoryContents (dbpath++"users")

recordPath :: Record -> FilePath
recordPath = (dbpath++) . recordPath'
    where
	recordPath' (RecPassword name) = userDir name ++ "passwd"
	recordPath' (RecUserInfo name) = userDir name ++ "info"
	recordPath' (RecUserInfoLog name) = userDir name ++ "log"
	recordPath' (RecLock ls) = locksDir ++ show ls
	recordPath' (RecNote (NoteInfo name _  alock)) = userDir name ++ "notes" ++ [pathSeparator] ++ alockFN alock
	recordPath' RecLockHashes = "lockHashes"
	recordPath' RecServerInfo = "serverInfo"

	userDir name = "users" ++ [pathSeparator] ++ pathifyName name ++ [pathSeparator]
	pathifyName = map (\c -> case c of {'/'->'s'; '.'->'d'; '\\'->'b'; _ -> c})
	alockFN (ActiveLock name idx) = pathifyName name ++":"++ show idx
	locksDir = "locks"++[pathSeparator]
dbpath = "intricacydb"++[pathSeparator]
