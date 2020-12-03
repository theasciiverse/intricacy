-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Cache where

import Network.Fancy
import Control.Concurrent.STM
import Control.Concurrent
import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Reader
import System.Directory
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
    deriving (Eq, Show)


getRecordCached :: ServerAddr -> Maybe Auth -> Maybe (TVar Bool) -> Bool -> Record -> IO (TVar FetchedRecord)
getRecordCached saddr _ _ _ _ | nullSaddr saddr = do
    newTVarIO (FetchedRecord True (Just "No server set.") Nothing)
getRecordCached saddr auth mflag cOnly rec = do
    fromCache <- withCache saddr $ getRecord rec
    let fresh = isJust fromCache && invariantRecord rec
    tvar <- newTVarIO (FetchedRecord fresh Nothing fromCache)
    unless (cOnly || fresh) $ void $ forkIO $ getRecordFromServer fromCache tvar
    return tvar
    where
        getRecordFromServer fromCache tvar = do
            let action = case rec of
                    RecUserInfo name ->
                        let curVersion = (\(RCUserInfo (v,_)) -> v) <$> fromCache
                        in GetUserInfo name curVersion
                    _ -> askForRecord rec
            resp <- makeRequest saddr (ClientRequest
                    protocolVersion (if needsAuth action then auth else Nothing) action)
            case resp of
                ServerError err -> tellRec $ FetchedRecord True (Just err) fromCache
                ServerCodenameFree -> tellRec $ FetchedRecord True Nothing Nothing
                ServerFresh -> tellRec $ FetchedRecord True Nothing fromCache
                ServedUserInfoDeltas deltas -> do
                    let Just (RCUserInfo (v,info)) = fromCache
                    let rc = RCUserInfo (v+length deltas, applyDeltas info deltas)
                    withCache saddr $ putRecord rec rc
                    tellRec $ FetchedRecord True Nothing (Just rc)
                _ -> do
                    let rc = rcOfServerResp resp
                    withCache saddr $ putRecord rec rc
                    tellRec $ FetchedRecord True Nothing (Just rc)
            where
                tellRec fr = atomically $ do
                    writeTVar tvar fr
                    case mflag of {Just flag -> writeTVar flag True; _ -> return ()}

waitFetchedFresh :: TVar FetchedRecord -> IO ()
waitFetchedFresh tvar = atomically $ readTVar tvar >>= check.fresh

makeRequest :: ServerAddr -> ClientRequest -> IO ServerResponse
makeRequest saddr _ | nullSaddr saddr =
    return $ ServerError "No server set."
makeRequest saddr@(ServerAddr host port) request =
    handle (return . ServerError . (show::SomeException -> String)) $
        withStream (IP host port) makeRequest'
            `catchIO` const (return $ ServerError $ "Cannot connect to "++saddrStr saddr++"!")
    where
        makeRequest' hdl = do
            BS.hPut hdl $ BL.toStrict $ encode request
            hFlush hdl
            decode . BL.fromStrict <$> BS.hGetContents hdl

knownServers :: IO [ServerAddr]
knownServers = flip catchIO (const $ return []) $ do
    cachedir <- confFilePath "cache"
    saddrstrs <- getDirectoryContents cachedir >>= filterM (\dir ->
        doesFileExist $ cachedir++[pathSeparator]++dir++[pathSeparator]++"serverInfo")
    return $ mapMaybe strToSaddr saddrstrs

withCache :: ServerAddr -> DBM a -> IO a
withCache saddr m = do
    cachedir <- (++pathSeparator : saddrPath saddr) <$> confFilePath "cache"
    runReaderT m cachedir
