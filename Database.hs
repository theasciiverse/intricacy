module Database where

-- TODO: use ByteString everywhere

import Control.Monad
import System.IO
import System.FilePath
import System.Directory
import Data.Char
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as CS

--FIXME: should be using OpenSSL.EVP.Digest from HsOpenSSL?
import OpenSSL.Digest (MessageDigest(SHA1), toHex)
import OpenSSL.Digest.ByteString.Lazy (digest)

import Protocol
import Metagame
import Lock
import Frame
import Mundanities

hash :: String -> IO String
hash str = fmap (>>=toHex) $ digest SHA1 $ CL.pack $ str

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

rcOfServerResp (ServedServerInfo x) = RCServerInfo x
rcOfServerResp (ServedLock x) = RCLock x
rcOfServerResp (ServedSolution x) = RCSolution x
rcOfServerResp (ServedUserInfo x) = RCUserInfo x
	    
recordExists :: Record -> IO Bool
recordExists = doesFileExist . recordPath

getRecord :: Record -> IO RecordContents
getRecord rec = do
    h <- openFile (recordPath rec) ReadMode
    rc <- getRecordh rec h
    hClose h
    return rc
getRecordh (RecPassword _) h = liftM RCPassword $ read `liftM` hGetStrict h
getRecordh (RecUserInfo _) h = liftM RCUserInfo $ read `liftM` hGetStrict h
getRecordh (RecLock _) h = liftM RCLock $ read `liftM` hGetStrict h
getRecordh (RecNote _) h = liftM RCSolution $ read `liftM` hGetStrict h
getRecordh (RecServerInfo) h = liftM RCServerInfo $ read `liftM` hGetStrict h

hGetStrict h = CS.unpack `liftM` concatMWhileNonempty (repeat $ CS.hGet h 1024)
    where concatMWhileNonempty (m:ms) = do
	    bs <- m
	    if CS.null bs
		then return bs
		else (bs `CS.append`) `liftM` concatMWhileNonempty ms

putRecord :: Record -> RecordContents -> IO ()
putRecord rec rc = do
    let path = recordPath rec
    mkdirhier path 
    h <- openFile path WriteMode 
    putRecordh rc h
    hClose h
putRecordh (RCPassword hpw) h = hPutStr h $ show hpw
putRecordh (RCUserInfo info) h = hPutStr h $ show info
putRecordh (RCLock lock) h = hPutStr h $ show lock
putRecordh (RCSolution solution) h = hPutStr h $ show solution
putRecordh (RCServerInfo sinfo) h = hPutStr h $ show sinfo

modifyRecord :: Record -> (RecordContents -> RecordContents) -> IO ()
modifyRecord rec f = do
    h <- openFile (recordPath rec) ReadWriteMode 
    rc <- getRecordh rec h
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

-- | TODO: use System.Directory functions to make this OS-independent
recordPath :: Record -> FilePath
recordPath = (dbpath++) . recordPath'
    where
	recordPath' (RecPassword name) = userDir name ++ "passwd"
	recordPath' (RecUserInfo name) = userDir name ++ "info"
	recordPath' (RecLock ls) = locksDir ++ show ls
	recordPath' (RecNote (NoteInfo name _  alock)) = userDir name ++ "notes/" ++ alockFN alock
	recordPath' (RecServerInfo) = "serverInfo"

	userDir name = "users/" ++ pathifyName name ++ "/"
	pathifyName = map (\c -> case c of {'/'->'s'; '.'->'d'; '\\'->'b'; _ -> c})
	alockFN (ActiveLock name idx) = pathifyName name++":"++(show idx)
	locksDir = "locks/"
dbpath = "intricacydb/"
