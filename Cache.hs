module Cache where

import Control.Concurrent.STM
import System.Directory

newtype ServerAddr = ServerAddr {hostname::String, port::Int}

defaultPort=27001 -- == ('i'<<8) + 'y'
saddrStr (ServerAddr hostname port) = hostname ++ if port==defaultPort then "" else ':':show port

getRecordCached :: ServerAddr -> Auth -> Record -> TVar (Either String RecordContents) -> IO ()
getRecordCached saddr auth rec tvar = do
    cdToCacheDir saddr
    gotCache <- (getRecord rec >>= (callBack . Right) >> return True)
	    `catchIOError` (\_ -> return False)
    unless (gotCache && invariantRecord rec) $ do
    resp <- makeRequest saddr (ClientRequest protocolVersion (Just auth) action)
    case resp of
	ServerError err -> callback $ Left err
	_ -> do
	    let rc = rcOfServerResp 
	    putRecord rec rc
	    callback $ Right rc
    where
	handleResponse (ServerError err) = Left err
	handleResponse (ServedServerInfo sinfo) = Right $ RC
	callback = atomically . writeTVar tvar


makeRequest :: ServerAddr -> ClientRequest -> IO ServerResponse
makeRequest (ServerAddr host port) request = withStream (IP host port) makeRequest'
    where makeRequest' handle =
	L.hPut handle $ encode request 
	hFlush handle
	return $ decode `liftM` L.hGetContents handle


-- | cheap way to get around having to pass the server address to the database
-- functions - we just cd to the right place before using them.
cdToCacheDir saddr = do
    confdir <- (++pathSeparator++saddrStr saddr) `liftM` confFilePath "cache"
    mkdirhier confdir
    setCurrentDirectory confdir
