-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
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
import Safe

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
import Util

class (Applicative m, MonadIO m) => UIMonad m where
    runUI :: m a -> IO a
    initUI :: m Bool
    endUI :: m ()
    drawMainState :: MainStateT m ()
    reportAlerts :: GameState -> [Alert] -> m ()
    drawMessage :: String -> m ()
    drawPrompt :: Bool -> String -> m ()
    endPrompt :: m ()
    drawError :: String -> m ()
    showHelp :: InputMode -> HelpPage -> m Bool
    getInput :: InputMode -> m [ Command ]
    getChRaw :: m ( Maybe Char )
    unblockInput :: m (IO ())
    setUIBinding :: InputMode -> Command -> Char -> m ()
    getUIBinding :: InputMode -> Command -> m String
    getDrawImpatience :: m ( Int -> IO () )
    toggleColourMode :: m ()
    warpPointer :: HexPos -> m ()
    setYNButtons :: m ()
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
	, rsLastAlerts::[Alert]
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
	, asyncInvalidate :: TVar (Maybe Codenames)
	, randomCodenames :: TVar [Codename]
	, userInfoTVs :: Map Codename (TVar FetchedRecord, UTCTime)
	, indexedLocks :: Map LockSpec (TVar FetchedRecord)
	, retiredLocks :: Maybe [LockSpec]
	, curLockPath :: FilePath
	, curLock :: Maybe (Lock,Maybe Solution)
	, listOffset :: Int
	}

type MainStateT = StateT MainState

data HelpPage = HelpPageInput | HelpPageGame
    deriving (Eq, Ord, Show, Enum)

ms2im :: MainState -> InputMode
ms2im mainSt = case mainSt of
    PlayState {} -> IMPlay
    ReplayState {} -> IMReplay
    EditState {} -> IMEdit
    MetaState {} -> IMMeta

newPlayState (frame,st) title sub = PlayState st frame [] False False [] [] title sub Map.empty
newReplayState st soln title = ReplayState st [] soln [] title Map.empty
newEditState (frame,st) msoln mpath = EditState [st] [] frame mpath
    ((\s->(st,s))<$>msoln) (Just (st, isJust msoln)) Nothing (PHS zero) (PHS zero) Map.empty
initMetaState = do
    flag <- atomically $ newTVar False
    errtvar <- atomically $ newTVar Nothing
    invaltvar <- atomically $ newTVar Nothing
    rnamestvar <- atomically $ newTVar []
    (saddr, auth, path) <- confFilePath "metagame.conf" >>=
	liftM (fromMaybe (defaultServerAddr, Nothing, "")) . readReadFile
    let names = maybeToList $ authUser <$> auth
    undecls <- if nullSaddr saddr then return [] else
	confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>=
	liftM (fromMaybe []) . readReadFile
    mlock <- fullLockPath path >>= readLock
    return $ MetaState saddr undecls False auth names flag errtvar invaltvar rnamestvar Map.empty Map.empty Nothing path mlock 0

readLock :: FilePath -> IO (Maybe (Lock, Maybe Solution))
readLock path = runMaybeT $ msum
	[ (\l->(l,Nothing)) <$> (MaybeT $ readReadFile path)
	, do
	    (mlock,msoln) <- lift $ readAsciiLockFile path
	    lock <- liftMaybe mlock
	    return $ (lock,msoln) ]
-- writeLock :: FilePath -> Lock -> IO ()
-- writeLock path lock = fullLockPath path >>= flip writeReadFile lock

writeMetaState (MetaState { curServer=saddr, undeclareds=undecls, curAuth=auth, curLockPath=path }) = do
    confFilePath "metagame.conf" >>= flip writeReadFile (saddr, auth, path)
    unless (nullSaddr saddr) $
	confFilePath ("undeclared" ++ [pathSeparator] ++ saddrStr saddr) >>= flip writeReadFile undecls

getTitle :: UIMonad uiM => MainStateT uiM (Maybe String)
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

editStateUnsaved :: UIMonad uiM => MainStateT uiM Bool
editStateUnsaved = (isNothing <$>) $ runMaybeT $ do
    (sst,tested) <- MaybeT $ gets lastSavedState
    st <- MaybeT $ gets $ headMay.esGameStateStack
    guard $ sst == st
    nowTested <- isJust <$> lift getCurTestSoln
    guard $ tested == nowTested

getCurTestSoln :: UIMonad uiM => MainStateT uiM (Maybe Solution)
getCurTestSoln = runMaybeT $ do
    (st',soln) <- MaybeT $ gets esTested
    st <- MaybeT $ gets $ headMay.esGameStateStack
    guard $ st == st'
    return soln

instance Error () where noMsg = ()

mgetOurName :: (UIMonad uiM) => MaybeT (MainStateT uiM) Codename
mgetOurName = MaybeT $ (authUser <$>) <$> gets curAuth
mgetCurName :: (UIMonad uiM) => MaybeT (MainStateT uiM) Codename
mgetCurName = MaybeT $ listToMaybe <$> gets codenameStack

getUInfoFetched :: UIMonad uiM => Integer -> Codename -> MainStateT uiM FetchedRecord
getUInfoFetched staleTime name = do
    uinfott <- gets (Map.lookup name . userInfoTVs)
    ($uinfott) $ maybe set $ \(tvar,time) -> do
	now <- liftIO getCurrentTime
	if floor (diffUTCTime now time) > staleTime
	    then set
	    else liftIO $ atomically $ readTVar tvar
    where
	set = do
	    now <- liftIO getCurrentTime
	    tvar <- getRecordCachedFromCur True $ RecUserInfo name
	    modify $ \ms -> ms {userInfoTVs = Map.insert name (tvar, now) $ userInfoTVs ms}
	    liftIO $ atomically $ readTVar tvar

mgetUInfo :: UIMonad uiM => Codename -> MaybeT (MainStateT uiM) UserInfo
mgetUInfo name = do
    RCUserInfo (_,uinfo) <- MaybeT $ (fetchedRC <$>) $ getUInfoFetched defaultStaleTime name
    return uinfo
    where defaultStaleTime = 300


invalidateUInfo :: UIMonad uiM => Codename -> MainStateT uiM ()
invalidateUInfo name =
    modify $ \ms -> ms {userInfoTVs = Map.delete name $ userInfoTVs ms}

invalidateAllUInfo :: UIMonad uiM => MainStateT uiM ()
invalidateAllUInfo =
    modify $ \ms -> ms {userInfoTVs = Map.empty}

data Codenames = AllCodenames | SomeCodenames [Codename]

invalidateUInfos :: UIMonad uiM => Codenames -> MainStateT uiM ()
invalidateUInfos AllCodenames = invalidateAllUInfo
invalidateUInfos (SomeCodenames names) = mapM_ invalidateUInfo names


mgetLock :: UIMonad uiM => LockSpec -> MaybeT (MainStateT uiM) Lock
mgetLock ls = do
    tvar <- msum [ MaybeT $ (Map.lookup ls) <$> gets indexedLocks
	, lift $ do
	    tvar <- getRecordCachedFromCur True $ RecLock ls
	    modify $ \ms -> ms { indexedLocks = Map.insert ls tvar $ indexedLocks ms }
	    return tvar ]
    RCLock lock <- MaybeT $ (fetchedRC<$>) $ liftIO $ atomically $ readTVar tvar
    return $ reframe lock

refreshUInfoUI :: (UIMonad uiM) => MainStateT uiM ()
refreshUInfoUI = void.runMaybeT $ do
    modify $ \ms -> ms { listOffset = 0 }
    mourNameSelected >>? getRandomNames
    lift $ modify $ \ms -> ms {retiredLocks = Nothing}
    lift.lift $ drawMessage ""
    where
	getRandomNames = do
	    rnamestvar <- gets randomCodenames
	    liftIO $ atomically $ writeTVar rnamestvar []
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

mourNameSelected :: (UIMonad uiM) => MaybeT (MainStateT uiM) Bool
mourNameSelected = liftM2 (==) mgetCurName mgetOurName

purgeInvalidUndecls :: (UIMonad uiM) => MainStateT uiM ()
purgeInvalidUndecls = do
    undecls' <- gets undeclareds >>= filterM ((not<$>).invalid)
    modify $ \ms -> ms { undeclareds = undecls' }
    where
	invalid (Undeclared _ ls (ActiveLock name idx)) =
	    (fromMaybe False <$>) $ runMaybeT $ do
		uinfo <- mgetUInfo name
		ourName <- mgetOurName
		(`mplus` return True) $ do
		    linfo <- liftMaybe $ userLocks uinfo ! idx
		    return $ public linfo
			|| ourName `elem` accessedBy linfo
			|| lockSpec linfo /= ls


curServerAction :: UIMonad uiM => Protocol.Action -> MainStateT uiM ServerResponse
curServerAction act = do
    saddr <- gets curServer
    auth <- gets curAuth
    cOnly <- gets cacheOnly
    if cOnly then return $ ServerError "Can't contact server in cache-only mode"
	else lift $ withImpatience $ liftIO $ makeRequest saddr $ ClientRequest protocolVersion auth act

curServerActionAsyncThenInvalidate :: UIMonad uiM => Protocol.Action -> Maybe Codenames -> MainStateT uiM ()
curServerActionAsyncThenInvalidate act names = do
    saddr <- gets curServer
    auth <- gets curAuth
    flag <- gets newAsync
    errtvar <- gets asyncError
    invaltvar <- gets asyncInvalidate
    cOnly <- gets cacheOnly
    void $ liftIO $ forkIO $ do
	resp <- if cOnly then return $ ServerError "Can't contact server in cache-only mode"
		else makeRequest saddr $ ClientRequest protocolVersion auth act
	case resp of
	    ServerError err -> atomically $ writeTVar errtvar $ Just err
	    _ -> atomically $ writeTVar invaltvar names
	atomically $ writeTVar flag True

checkAsync :: UIMonad uiM => MainStateT uiM ()
checkAsync = do
    void.runMaybeT $ do
	errtvar <- lift $ gets asyncError
	err <- MaybeT $ liftIO $ atomically $
		readTVar errtvar <* writeTVar errtvar Nothing
	lift.lift $ drawError err
    void.runMaybeT $ do
	invaltvar <- lift $ gets asyncInvalidate
	names <- MaybeT $ liftIO $ atomically $
		readTVar invaltvar <* writeTVar invaltvar Nothing
	lift $ invalidateUInfos names >> refreshUInfoUI

getRecordCachedFromCur :: UIMonad uiM => Bool -> Record -> MainStateT uiM (TVar FetchedRecord)
getRecordCachedFromCur flagIt rec = do
    saddr <- gets curServer
    auth <- gets curAuth
    cOnly <- gets cacheOnly
    flag <- gets newAsync
    liftIO $ getRecordCached saddr auth
	(if flagIt then Just flag else Nothing) cOnly rec

getFreshRecBlocking :: UIMonad uiM => Record -> MainStateT uiM (Maybe RecordContents)
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


getRelScore :: (UIMonad uiM) => Codename -> MainStateT uiM (Maybe Int)
getRelScore name = (fst<$>) <$> getRelScoreDetails name
getRelScoreDetails name = runMaybeT $ do
    ourName <- mgetOurName
    guard $ ourName /= name
    uinfo <- mgetUInfo name
    ourUInfo <- mgetUInfo ourName
    let (neg,pos) = (countPoints ourUInfo uinfo, countPoints uinfo ourUInfo)
    return $ (pos-neg,(pos,neg))
    where
	countPoints mugu masta = length $ filter (maybe False winsPoint) $ getAccessInfo mugu masta

accessedAL :: (UIMonad uiM) => ActiveLock -> MainStateT uiM Bool
accessedAL (ActiveLock name idx) = (isJust <$>) $ runMaybeT $ do
    ourName <- mgetOurName
    guard $ ourName /= name
    uinfo <- mgetUInfo name
    ourUInfo <- mgetUInfo ourName
    guard $ isJust $ getAccessInfo uinfo ourUInfo !! idx

getNotesReadOn :: UIMonad uiM => LockInfo -> MainStateT uiM [NoteInfo]
getNotesReadOn lockinfo = (fromMaybe [] <$>) $ runMaybeT $ do
    ourName <- mgetOurName
    ourUInfo <- mgetUInfo ourName
    return $ filter (\n -> isNothing (noteBehind n)
	    || n `elem` notesRead ourUInfo) $ lockSolutions lockinfo

testAuth :: UIMonad uiM => MainStateT uiM ()
testAuth = isJust <$> gets curAuth >>? do
    resp <- curServerAction $ Authenticate
    case resp of
	ServerMessage msg -> (lift $ drawMessage $ "Server: " ++ msg)
	ServerError err -> do
	    lift $ drawMessage err
	    modify $ \ms -> ms {curAuth = Nothing}
	_ -> return ()

metagameHelpText :: [String]
metagameHelpText =
    [ "By ruthlessly guarded secret arrangement, the Council's agents can pick any lock in the city."
    , "The Guild produces the necessary locks - apparently secure, but with fatal hidden flaws."
    , "The ritual game known as \"Intricacy\" is played to determine the best designs."
    , "Players attempt to design locks which can be picked only by one who knows the secret,"
    , "and try to discover the secret flaws in the designs of their colleagues."
    , ""
    , "You may put forward up to three prototype locks. They will guard the secrets you discover."
    , "If you pick a colleague's lock, the rules require that a note be written describing your solution."
    , "The composition and deciphering of notes is an art in itself, whose details do not concern us here,"
    , "but a note proves that the author found a solution, while revealing as little detail as possible."
    , "To declare your success, you must secure your note behind a lock of your own."
    , "If you are able to unlock a lock, you automatically read all the notes it secures."
    , "If you read three notes on a lock, you will piece together the clues and work out how to solve it."
    , ""
    , "Players are judged relative to each of their peers. There are no absolute rankings."
    , "You win a point of esteem against another player for one of their locks if either:"
    , "you have declared a note on the lock and the lock's owner has not read that note,"
    , "or if you have read three notes on the lock."
    , "Relative esteem ranges from +3 (best) to -3 (worst), and is calculated as"
    , "the number of their locks which win you a point minus the number of your locks which win them one."
    , "If the secrets to one of your locks become widely disseminated, you may wish to replace it."
    , "However: once replaced, a lock is \"retired\", and the notes it secured are read by everyone."
    ]
