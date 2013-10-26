module Cache where

import Network.Fancy
import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import Control.Exception
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
import Metagame
import Mundanities
import ServerAddr

data FetchedRecord = FetchedRecord {fresh :: Bool, fetchError :: Maybe String, fetchedRC :: Maybe RecordContents}
    deriving (Eq, Ord, Show)


getRecordCached :: ServerAddr -> Maybe Auth -> Maybe (TVar Bool) -> Bool -> Record -> IO (TVar FetchedRecord)
getRecordCached saddr auth mflag cOnly rec = withCacheDir saddr $ do
    fromCache <- (Just <$> getRecord rec) `catchIOError` (\_ -> return Nothing)
    let fresh = isJust fromCache && invariantRecord rec
    tvar <- atomically $ newTVar $ FetchedRecord fresh Nothing fromCache
    unless (cOnly || fresh) $ void $ forkIO $ withCacheDir saddr $ getRecordFromServer fromCache tvar
    return tvar
    where
	getRecordFromServer fromCache tvar = do
	    let action = case rec of
		    RecUserInfo name ->
			let curVersion = (\(RCUserInfo (v,_)) -> v) <$> fromCache
			in GetUserInfo name curVersion 
		    _ -> askForRecord rec
	    resp <- makeRequest saddr (ClientRequest protocolVersion auth action)
	    case resp of
		ServerError err -> tellRec $ FetchedRecord True (Just err) fromCache
		ServerCodenameFree -> tellRec $ FetchedRecord True Nothing Nothing
		ServerFresh -> tellRec $ FetchedRecord True Nothing fromCache
		ServedUserInfoDeltas deltas -> do
		    let Just (RCUserInfo (v,info)) = fromCache
		    let rc = RCUserInfo (v+length deltas, applyDeltas info deltas)
		    putRecord rec rc
		    tellRec $ FetchedRecord True Nothing (Just rc)
		_ -> do
		    let rc = rcOfServerResp resp
		    putRecord rec rc
		    tellRec $ FetchedRecord True Nothing (Just rc)
	    where
		tellRec fr = atomically $ do
		    writeTVar tvar fr
		    case mflag of {Just flag -> writeTVar flag True; _ -> return ()}

waitFetchedFresh :: TVar FetchedRecord -> IO ()
waitFetchedFresh tvar = atomically $ readTVar tvar >>= check.fresh

makeRequest :: ServerAddr -> ClientRequest -> IO ServerResponse
makeRequest saddr@(ServerAddr host port) request =
    handle (return . ServerError . (show::SomeException -> String)) $
	withStream (IP host port) makeRequest'
	    `catchIOError` (const $ return $ ServerError $ "Cannot connect to "++saddrStr saddr++"!")
	
    where
	makeRequest' handle = do
	    BS.hPut handle $ BL.toStrict $ encode request 
	    hFlush handle
	    (decode . BL.fromStrict) `liftM` BS.hGetContents handle


-- | cheap way to get around having to pass the server address to the database
-- functions - we just cd to the right place before using them.
withCacheDir saddr m = do
    dir <- getCurrentDirectory
    confdir <- (++[pathSeparator]++saddrStr saddr) `liftM` confFilePath "cache"
    mkdirhier confdir
    setCurrentDirectory confdir
    m <* setCurrentDirectory dir

