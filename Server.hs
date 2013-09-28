module Main where

--import Data.BinaryCom as BC
import Network.Fancy

import System.IO
import System.FilePath
import System.Directory
import Control.Monad
import Data.Char
import qualified Data.ByteString.Lazy as L
import Data.Binary

import Protocol
import Metagame
import Lock
import GameState

port = 27001 -- 27001 == ('i'<<8) + 'y'
main = do 
    streamServer serverSpec{address = IPv4 "" port} handler
    sleepForever

handler :: Handle -> Address -> IO ()
handler handle _ = do
    --com <- BC.binaryCom handle
    --request <- BC.receive com

    --request <- read `liftM` hGetContents handle

    request <- decode `liftM` L.hGetContents handle

    response <- handleRequest request

    L.hPut handle $ encode response

    --hPutStr handle $ show response

    --BC.send com response

handleRequest :: ClientRequest -> IO ServerResponse
handleRequest req@(ClientRequest pv mauth action) = do
    print req
    if pv /= protocolVersion
    then return $ ServerError "Bad protocol version"
    else case action of
	Authenticate -> case mauth of
	    Nothing -> return $ ServerError "No auth"
	    Just auth -> do
		ok <- checkAuth auth
		if ok then return ServerAck else return $ ServerError "Wrong password"
	_ -> return $ ServerError "BUG: bad request"

checkAuth :: Auth -> IO Bool
checkAuth (Auth name pw) = do
    if not $ validCodeName name then return False
    else do
    let rec = RecPassword name
    exists <- recordExists $ rec
    if exists then (RCPassword pw==) `liftM` getRecord rec
	else putRecord rec (RCPassword pw) >> return True

validCodeName name = length name == 3 && and (map (validChar) name)
    where validChar c = isPrint c && not (isLower c) && c /= ' '

data Record
    = RecPassword Codename
    | RecUserInfo Codename
    | RecLock LockSpec
    | RecNote Codename ActiveLock
    deriving (Eq, Ord, Show)
data RecordContents
    = RCPassword Password
    | RCUserInfo UserInfo
    | RCLock Lock
    | RCSolution Solution
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

putRecord :: Record -> RecordContents -> IO ()
putRecord rec rc = putRecord' rc
    where
	path = recordPath rec
	putFile str = mkdirhier path >> writeFile path str
	putRecord' (RCPassword pw) = putFile $ show pw
	putRecord' (RCUserInfo info) = putFile $ show info
	putRecord' (RCLock lock) = putFile $ show lock
	putRecord' (RCSolution solution) = putFile $ show solution

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

	userDir name = "users/" ++ pathifyName name ++ "/"
	pathifyName = map (\c -> case c of {'/'->'s'; '.'->'d'; '\\'->'b'; _ -> c})
	alockFN (ActiveLock name idx) = pathifyName name++":"++(show idx)
	locksDir = "locks/"
	dbpath = "intricacydb/"
