module Cache where

import Network.Fancy
import Control.Concurrent.STM
import Control.Concurrent
import System.Directory
import System.IO.Error
import Control.Monad
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary
import System.IO
import System.FilePath

import Database
import Protocol
import Mundanities
import ServerAddr

data FetchedRecord = FetchedRecord {fresh :: Bool, fetchError :: Maybe String, fetchedRC :: Maybe RecordContents}
    deriving (Eq, Ord, Show)


getRecordCached :: ServerAddr -> Maybe Auth -> Maybe (TVar Bool) -> Bool -> Record -> IO (TVar FetchedRecord)
getRecordCached saddr auth mflag cOnly rec = do
    tvar <- atomically $ newTVar $ FetchedRecord False Nothing Nothing
    forkIO $ getRecordCached' tvar
    return tvar
    where
	getRecordCached' tvar = do
	    cdToCacheDir saddr
	    fromCache <- (liftM Just $ getRecord rec) `catchIOError` (\_ -> return Nothing)
	    when (isJust fromCache) $ tellRec $ FetchedRecord (invariantRecord rec) Nothing fromCache
	    unless (cOnly || (isJust fromCache && invariantRecord rec)) $ do
	    resp <- makeRequest saddr (ClientRequest protocolVersion auth (askForRecord rec)) `catchIOError`
		(const $ return $ ServerError $ "Can not connect to "++saddrStr saddr++"!")
	    case resp of
		ServerError err -> tellRec $ FetchedRecord False (Just err) fromCache
		ServerCodenameFree -> tellRec $ FetchedRecord True Nothing Nothing
		_ -> do
		    let rc = rcOfServerResp resp
		    putRecord rec rc
		    tellRec $ FetchedRecord True Nothing (Just rc)
	    where
		tellRec fr = atomically $ do
		    writeTVar tvar fr 
		    case mflag of {Just flag -> writeTVar flag True; _ -> return ()}


makeRequest :: ServerAddr -> ClientRequest -> IO ServerResponse
makeRequest (ServerAddr host port) request = withStream (IP host port) makeRequest'
    where
	makeRequest' handle = do
	    BS.hPut handle $ BL.toStrict $ encode request 
	    hFlush handle
	    (decode . BL.fromStrict) `liftM` BS.hGetContents handle


-- | cheap way to get around having to pass the server address to the database
-- functions - we just cd to the right place before using them.
cdToCacheDir saddr = do
    confdir <- (++[pathSeparator]++saddrStr saddr) `liftM` confFilePath "cache"
    mkdirhier confdir
    setCurrentDirectory confdir
