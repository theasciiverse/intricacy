-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Database where

import Data.Maybe
import Data.Tuple (swap)
import Control.Applicative
import Control.Monad
import System.IO
import System.FilePath
import System.Directory
import Control.Monad.Trans.Reader
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.ByteString.Char8 as CS

import Crypto.Hash (hashlazy, Digest, SHA1, digestToHexByteString)

import Protocol
import Metagame
import Lock
import Mundanities

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
    | RecRetiredLocks Codename
    | RecServerInfo
    deriving (Eq, Ord, Show)
data RecordContents
    = RCPassword Password
    | RCUserInfo VersionedUInfo
    | RCUserInfoDeltas [UserInfoDelta]
    | RCLock Lock
    | RCSolution Solution
    | RCLockHashes [String]
    | RCLockSpecs [LockSpec]
    | RCServerInfo ServerInfo
    deriving (Eq, Ord, Show)

rcOfServerResp (ServedServerInfo x) = RCServerInfo x
rcOfServerResp (ServedLock x) = RCLock x
rcOfServerResp (ServedSolution x) = RCSolution x
rcOfServerResp (ServedUserInfo x) = RCUserInfo x
rcOfServerResp (ServedRetired x) = RCLockSpecs x
rcOfServerResp _ = error "no corresponding rc"

invariantRecord (RecUserInfo _) = False
invariantRecord (RecUserInfoLog _) = False
invariantRecord (RecPassword _) = False
invariantRecord (RecRetiredLocks _) = False
invariantRecord (RecNote _) = False
invariantRecord _ = True

askForRecord RecServerInfo = GetServerInfo
askForRecord (RecUserInfo name) = GetUserInfo name Nothing
askForRecord (RecLock ls) = GetLock ls
askForRecord (RecNote note) = GetSolution note
askForRecord (RecRetiredLocks name) = GetRetired name
askForRecord _ = error "no corresponding request"

type DBM = ReaderT FilePath IO
withDB :: FilePath -> DBM a -> IO a
withDB = flip runReaderT

recordExists :: Record -> DBM Bool
recordExists rec = recordPath rec >>= liftIO . doesFileExist

getRecord :: Record -> DBM (Maybe RecordContents)
getRecord rec = do
    path <- recordPath rec
    liftIO $ flip catchIO (const $ return Nothing) $ do
	h <- openFile path ReadMode
	getRecordh rec h <* hClose h
getRecordh (RecPassword _) h = ((RCPassword <$>) . tryRead) <$> hGetStrict h
getRecordh (RecUserInfo _) h = ((RCUserInfo <$>) . tryRead) <$> hGetStrict h
getRecordh (RecUserInfoLog _) h = ((RCUserInfoDeltas <$>) . tryRead) <$> hGetStrict h
getRecordh (RecLock _) h = ((RCLock <$>) . tryRead) <$> hGetStrict h
getRecordh (RecNote _) h = ((RCSolution <$>) . tryRead) <$> hGetStrict h
getRecordh RecLockHashes h = ((RCLockHashes <$>) . tryRead) <$> hGetStrict h
getRecordh (RecRetiredLocks name) h = ((RCLockSpecs <$>) . tryRead) <$> hGetStrict h
getRecordh RecServerInfo h = ((RCServerInfo <$>) . tryRead) <$> hGetStrict h

hGetStrict h = CS.unpack <$> concatMWhileNonempty (repeat $ CS.hGet h 1024)
    where concatMWhileNonempty (m:ms) = do
	    bs <- m
	    if CS.null bs
		then return bs
		else (bs `CS.append`) <$> concatMWhileNonempty ms

putRecord :: Record -> RecordContents -> DBM ()
putRecord rec rc = do
    path <- recordPath rec
    liftIO $ do
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
putRecordh (RCLockSpecs lss) h = hPutStr h $ show lss
putRecordh (RCServerInfo sinfo) h = hPutStr h $ show sinfo

modifyRecord :: Record -> (RecordContents -> RecordContents) -> DBM ()
modifyRecord rec f = do
    h <- recordPath rec >>= liftIO . flip openFile ReadWriteMode
    liftIO $ do
	Just rc <- getRecordh rec h
	hSeek h AbsoluteSeek 0
	putRecordh (f rc) h
	hTell h >>= hSetFileSize h
	hClose h

delRecord :: Record -> DBM ()
delRecord rec = recordPath rec >>= liftIO . removeFile

newLockRecord :: Lock -> DBM LockSpec
newLockRecord lock = do
    dbpath <- ask
    let path = dbpath++[pathSeparator]++"lastlock"
    h <- liftIO $ openFile path ReadWriteMode
    contents <- liftIO $ hGetStrict h
    let ls = if null contents then 0 else 1 + read contents
    liftIO $ hSeek h AbsoluteSeek 0
    liftIO $ hPutStr h $ show ls
    liftIO $ hClose h
    putRecord (RecLock ls) (RCLock lock)
    return ls

listUsers :: DBM [Codename]
listUsers = do
    dbpath <- ask
    liftIO $ (map unpathifyName . filter ((==3).length)) <$>
	getDirectoryContents (dbpath++[pathSeparator]++"users")

recordPath :: Record -> DBM FilePath
recordPath rec =
    (++ ([pathSeparator] ++ recordPath' rec)) <$> ask
    where
	recordPath' (RecPassword name) = userDir name ++ "passwd"
	recordPath' (RecUserInfo name) = userDir name ++ "info"
	recordPath' (RecUserInfoLog name) = userDir name ++ "log"
	recordPath' (RecLock ls) = locksDir ++ show ls
	recordPath' (RecNote (NoteInfo name _  alock)) =
	    userDir name ++ "notes" ++ [pathSeparator] ++ alockFN alock
	recordPath' (RecRetiredLocks name) = userDir name ++ "retired"
	recordPath' RecLockHashes = "lockHashes"
	recordPath' RecServerInfo = "serverInfo"

	userDir name = "users" ++ [pathSeparator] ++ pathifyName name ++ [pathSeparator]
	alockFN (ActiveLock name idx) = pathifyName name ++":"++ show idx
	locksDir = "locks"++[pathSeparator]

-- Dummy out characters which are disallowed on unix or dos.
-- We use lowercase characters as dummies.
-- To avoid collisions on case-insensitive filesystems, we use '_' as an
-- escape character.
pathifyName = concatMap $ \c ->
    fromMaybe [c] (('_':) . pure <$> lookup c pathifyAssocs)
unpathifyName = concatMap $ \c -> case c of
	'_' -> ""
	_ -> pure $ fromMaybe c (lookup c $ map swap pathifyAssocs)
pathifyAssocs =
    [ ('/','s')
    , ('.','d')
    , ('\\','b')
    , ('<','l')
    , ('>','g')
    , (':','c')
    , ('|','p')
    , ('?','q')
    , ('*','a')
    , ('+','t')
    , (',','m')
    , (';','i')
    , ('=','e')
    , ('[','k')
    , (']','j')
    , ('_','u')
    ]
