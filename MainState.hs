-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module MainState where

import Control.Monad.State
import Control.Applicative
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Char
import Data.List
import Control.Concurrent.STM
import Control.Concurrent
import System.Directory
import System.FilePath
import Data.Time.Clock
import Data.Array
import Data.Function (on)

import Hex
import Mundanities
import AsciiLock
import GameStateTypes
import Physics
import Command
import Frame
import Lock
import Cache
import Database
import Protocol
import Metagame
import ServerAddr
import InputMode

class (Applicative m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawMainState :: StateT MainState m ()
    drawAlerts :: [Alert] -> m ()
    drawMessage :: String -> m ()
    drawError :: String -> m ()
    showHelp :: InputMode -> m ()
    getInput :: InputMode -> m [ Command ]
    unblockInput :: m (IO ())
    getDrawImpatience :: m ( Int -> IO () )
    suspend,redraw :: m ()

    doUI :: m a -> IO (Maybe a)
    doUI m = runUI $ do
	ok <- initUI
	if ok then m >>= (endUI >>).return.Just else return Nothing

-- | this could be neatened using GADTs
data MainState 
    = PlayState
	{ psCurrentState::GameState
	, psFrame::Frame
	, psLastAlerts::[Alert]
	, wrenchSelected::Bool
	, psSolved::Bool
	, psGameStateMoveStack::[(GameState, PlayerMove)]
	, psUndoneStack::[(GameState, PlayerMove)]
	, psTitle::Maybe String
	, psIsSub::Bool
	, psMarks::Map Char MainState
	}
    | ReplayState
	{ rsCurrentState::GameState
	, rsMoveStack::[PlayerMove]
	, rsGameStateMoveStack::[(GameState, PlayerMove)]
	, rsTitle::Maybe String
	, rsMarks::Map Char MainState
	}
    | EditState
	{ esGameStateStack::[GameState]
	, esUndoneStack::[GameState]
	, esFrame::Frame
	, esPath::Maybe FilePath
	, esTested::Maybe (GameState,Solution)
	, lastSavedState::Maybe (GameState, Bool)
	, selectedPiece::Maybe PieceIdx
	, selectedPos::HexPos
	, lastModPos::HexPos
	, esMarks::Map Char GameState
	}
    | MetaState
	{ curServer :: ServerAddr
	, undeclareds :: [Undeclared]
	, cacheOnly :: Bool
	, curAuth :: Maybe Auth
	, codenameStack :: [Codename]
	, newAsync :: TVar Bool
	, asyncError :: TVar (Maybe String)
	, randomCodenames :: TVar [Codename]
	, userInfos :: Map Codename (TVar FetchedRecord, UTCTime)
	, indexedLocks :: Map LockSpec (TVar FetchedRecord)
	, retiredLocks :: Maybe [LockSpec]
	, curLockPath :: FilePath
	, curLock :: Maybe (Lock,Maybe Solution)
	, listOffset :: Int
	}

newPlayState (frame,st) title sub = PlayState st frame [] False False [] [] title sub Map.empty
newReplayState st soln title = ReplayState st soln [] title Map.empty
newEditState (frame,st) msoln mpath = EditState [st] [] frame mpath
    ((\s->(st,s))<$>msoln) (Just (st, isJust msoln)) Nothing (PHS zero) (PHS zero) Map.empty
initMetaState = do
    flag <- atomically $ newTVar False
    errtvar <- atomically $ newTVar Nothing
    rnamestvar <- atomically $ newTVar []
    (saddr, auth, path) <- confFilePath "metagame.conf" >>=
	liftM (fromMaybe (defaultServerAddr, Nothing, "")) . readReadFile
    let names = maybeToList $ authUser <$> auth
    undecls <- if nullSaddr saddr then return [] else
	confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>=
	liftM (fromMaybe []) . readReadFile
    mlock <- fullLockPath path >>= readLock
    return $ MetaState saddr undecls False auth names flag errtvar rnamestvar Map.empty Map.empty Nothing path mlock 0

readLock :: FilePath -> IO (Maybe (Lock, Maybe Solution))
readLock path = runMaybeT $ msum
	[ (\l->(l,Nothing)) <$> (MaybeT $ readReadFile path)
	, do
	    (mlock,msoln) <- lift $ readAsciiLockFile path
	    lock <- MaybeT $ return mlock
	    return $ (lock,msoln) ]
-- writeLock :: FilePath -> Lock -> IO ()
-- writeLock path lock = fullLockPath path >>= flip writeReadFile lock
fullLockPath path = if isAbsolute path
    then return path
    else (++(pathSeparator:path)) <$> confFilePath "locks"

writeMetaState (MetaState { curServer=saddr, undeclareds=undecls, curAuth=auth, curLockPath=path }) = do
    confFilePath "metagame.conf" >>= flip writeReadFile (saddr, auth, path)
    unless (nullSaddr saddr) $
	confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>= flip writeReadFile undecls

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState {} -> IMPlay
    ReplayState {} -> IMReplay
    EditState {} -> IMEdit
    MetaState {} -> IMMeta

getTitle :: UIMonad uiM => StateT MainState uiM (Maybe String)
getTitle = ms2im <$> get >>= \im -> case im of
    IMEdit -> do
	mpath <- gets esPath
	unsaved <- editStateUnsaved
	isTested <- isJust <$> getCurTestSoln
	return $ Just $ "editing " ++ fromMaybe "[unnamed lock]" mpath ++ 
	    (if isTested then " (Tested)" else "") ++
	    (if unsaved then " [+]" else "    ")
    IMPlay -> gets psTitle
    IMReplay -> gets rsTitle
    _ -> return Nothing

editStateUnsaved :: UIMonad uiM => StateT MainState uiM Bool
editStateUnsaved = (isNothing <$>) $ runMaybeT $ do
    (sst,tested) <- MaybeT $ gets lastSavedState
    st <- lift $ gets $ head.esGameStateStack
    guard $ sst == st
    nowTested <- isJust <$> lift getCurTestSoln
    guard $ tested == nowTested

getCurTestSoln :: UIMonad uiM => StateT MainState uiM (Maybe Solution)
getCurTestSoln = runMaybeT $ do
    (st',soln) <- MaybeT $ gets esTested
    st <- lift $ gets $ head.esGameStateStack
    guard $ st == st'
    return soln

instance Error () where noMsg = ()

mgetOurName :: (UIMonad uiM) => MaybeT (StateT MainState uiM) Codename
mgetOurName = MaybeT $ (authUser <$>) <$> gets curAuth
mgetCurName :: (UIMonad uiM) => MaybeT (StateT MainState uiM) Codename
mgetCurName = MaybeT $ listToMaybe <$> gets codenameStack 

getUInfoFetched :: UIMonad uiM => Integer -> Codename -> StateT MainState uiM FetchedRecord
getUInfoFetched staleTime name =
    gets ((Map.lookup name) . userInfos) >>=
	\x -> case x of
	    Nothing -> set
	    Just (tvar, time) -> do
		now <- liftIO getCurrentTime
		if floor (diffUTCTime now time) > staleTime
		    then set
		    else liftIO $ atomically $ readTVar tvar
    where set = do
	    tvar <- setUInfoTVar name
	    liftIO $ atomically $ readTVar tvar

mgetUInfo :: UIMonad uiM => Codename -> MaybeT (StateT MainState uiM) UserInfo
mgetUInfo name = do
    RCUserInfo (_,uinfo) <- MaybeT $ (fetchedRC <$>) $ getUInfoFetched defaultStaleTime name
    return uinfo
    where defaultStaleTime = 300

setUInfoTVar :: UIMonad uiM => Codename -> StateT MainState uiM (TVar FetchedRecord)
setUInfoTVar name = do
    now <- liftIO getCurrentTime
    tvar <- getRecordCachedFromCur True $ RecUserInfo name
    modify $ \ms -> ms {userInfos = Map.insert name (tvar, now) $ userInfos ms}
    return tvar

invalidateUInfo :: UIMonad uiM => Codename -> StateT MainState uiM ()
invalidateUInfo name = 
    modify $ \ms -> ms {userInfos = Map.delete name $ userInfos ms}

mgetLock :: UIMonad uiM => LockSpec -> MaybeT (StateT MainState uiM) Lock
mgetLock ls = do
    tvar <- msum [ MaybeT $ (Map.lookup ls) <$> gets indexedLocks 
	, lift $ do
	    tvar <- getRecordCachedFromCur True $ RecLock ls 
	    modify $ \ms -> ms { indexedLocks = Map.insert ls tvar $ indexedLocks ms }
	    return tvar ]
    RCLock lock <- MaybeT $ (fetchedRC<$>) $ liftIO $ atomically $ readTVar tvar
    return $ reframe lock

refreshUInfoUI :: (UIMonad uiM) => StateT MainState uiM ()
refreshUInfoUI = void.runMaybeT $ do
    modify $ \ms -> ms { listOffset = 0 }
    mourNameSelected >>= flip when getRandomNames
    lift $ modify $ \ms -> ms {retiredLocks = Nothing}
    lift.lift $ drawMessage ""
    where
	getRandomNames = do
	    rnamestvar <- gets randomCodenames
	    flag <- gets newAsync
	    saddr <- gets curServer
	    void $ liftIO $ forkIO $ do
		resp <- makeRequest saddr $
		    ClientRequest protocolVersion Nothing $ GetRandomNames 19
		case resp of
		    ServedRandomNames names -> atomically $ do 
			    writeTVar rnamestvar names
			    writeTVar flag True
		    _ -> return ()

mourNameSelected :: (UIMonad uiM) => MaybeT (StateT MainState uiM) Bool
mourNameSelected = liftM2 (==) mgetCurName mgetOurName

purgeInvalidUndecls :: (UIMonad uiM) => StateT MainState uiM ()
purgeInvalidUndecls = do
    undecls' <- gets undeclareds >>= filterM ((not<$>).invalid)
    modify $ \ms -> ms { undeclareds = undecls' }
    where
	invalid (Undeclared _ ls (ActiveLock name idx)) =
	    (fromMaybe False <$>) $ runMaybeT $ do
		uinfo <- mgetUInfo name
		ourName <- mgetOurName
		msum [ do
			linfo <- MaybeT $ return $ userLocks uinfo ! idx
			return $ public linfo 
			    || ourName `elem` accessedBy linfo 
			    || lockSpec linfo /= ls
		    , return True
		    ]

curServerAction :: UIMonad uiM => Protocol.Action -> StateT MainState uiM ServerResponse
curServerAction act = do
    saddr <- gets curServer
    auth <- gets curAuth
    lift $ withImpatience $ liftIO $ makeRequest saddr $ ClientRequest protocolVersion auth act

curServerActionAsync :: UIMonad uiM => Protocol.Action -> StateT MainState uiM ()
curServerActionAsync act = do
    saddr <- gets curServer
    auth <- gets curAuth
    flag <- gets newAsync
    errtvar <- gets asyncError
    void $ liftIO $ forkIO $ do
	resp <- makeRequest saddr $ ClientRequest protocolVersion auth act
	atomically $ writeTVar flag True
	case resp of
	    ServerError err -> atomically $ writeTVar errtvar $ Just err
	    _ -> return ()

checkAsyncErrors :: UIMonad uiM => StateT MainState uiM ()
checkAsyncErrors = void.runMaybeT $ do
    errtvar <- lift $ gets asyncError
    err <- MaybeT $ liftIO $ atomically $ readTVar errtvar
    lift.lift $ drawError err
    liftIO $ atomically $ writeTVar errtvar Nothing

getRecordCachedFromCur :: UIMonad uiM => Bool -> Record -> StateT MainState uiM (TVar FetchedRecord)
getRecordCachedFromCur flagIt rec = do
    saddr <- gets curServer
    auth <- gets curAuth
    cOnly <- gets cacheOnly
    flag <- gets newAsync
    liftIO $ getRecordCached saddr auth
	(if flagIt then Just flag else Nothing) cOnly rec

getFreshRecBlocking :: UIMonad uiM => Record -> StateT MainState uiM (Maybe RecordContents)
getFreshRecBlocking rec = do
    tvar <- getRecordCachedFromCur False rec
    cOnly <- gets cacheOnly
    fetched <- lift $ withImpatience $ liftIO $ atomically $ do
	fetched@(FetchedRecord fresh _ _) <- readTVar tvar
	check $ fresh || cOnly
	return fetched
    case fetchError fetched of
	Nothing -> return $ fetchedRC fetched
	Just err -> lift (drawError err) >> return Nothing

-- |draw indication that we're waiting for the server while we do so
withImpatience :: UIMonad uiM => uiM a -> uiM a
withImpatience m = do
    drawImpatience <- getDrawImpatience
    finishedTV <- liftIO $ atomically $ newTVar False
    let waitImpatiently ticks = do
	    wakeTV <- atomically $ newTVar False
	    forkIO $ threadDelay (10^6) >> atomically (writeTVar wakeTV True)
	    finished <- atomically $ do
		wake <- readTVar wakeTV
		finished <- readTVar finishedTV
		check $ wake || finished
		return finished
	    when (not finished) $ drawImpatience (ticks+1) >> waitImpatiently (ticks+1)
    liftIO $ forkIO $ waitImpatiently 0
    m <* (liftIO $ atomically $ writeTVar finishedTV True)

getRelScore :: (UIMonad uiM) => Codename -> StateT MainState uiM (Maybe Int)
getRelScore name = runMaybeT $ do
    ourName <- mgetOurName
    guard $ ourName /= name
    uinfo <- mgetUInfo name
    ourUInfo <- mgetUInfo ourName
    return $ countUnaccessedBy ourUInfo name - countUnaccessedBy uinfo ourName
    where
	countUnaccessedBy ui name = length $ filter (not.snd) $ getAccessInfo ui name 

getNotesReadOn :: UIMonad uiM => LockInfo -> StateT MainState uiM [NoteInfo]
getNotesReadOn lockinfo = (fromMaybe [] <$>) $ runMaybeT $ do
    ourName <- mgetOurName
    ourUInfo <- mgetUInfo ourName
    return $ filter (\n -> isNothing (noteBehind n)
	    || n `elem` notesRead ourUInfo) $ lockSolutions lockinfo

testAuth :: UIMonad uiM => StateT MainState uiM ()
testAuth = (isJust <$> gets curAuth >>=) $ flip when $ do
    resp <- curServerAction $ Authenticate
    case resp of
	ServerMessage msg -> (lift $ drawMessage $ "Server: " ++ msg)
	ServerError err -> do
	    lift $ drawMessage err
	    modify $ \ms -> ms {curAuth = Nothing}
	_ -> return ()
