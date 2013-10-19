module Protocol where

import Data.Binary
import Control.Monad

import BinaryInstances
import Metagame
import Lock

type ProtocolVersion = Int
protocolVersion = 1 :: ProtocolVersion

data ClientRequest = ClientRequest ProtocolVersion (Maybe Auth) Action
    deriving (Eq, Ord, Show, Read)

data Action
    = Authenticate
    | Register
    | GetServerInfo
    | GetLock LockSpec
    | GetUserInfo Codename
    | GetHint NoteInfo
    | GetSolution NoteInfo
    | DeclareSolution Solution LockSpec ActiveLock LockIndex
    | SetLock Lock LockIndex Solution
    deriving (Eq, Ord, Show, Read)

data Auth = Auth {authUser :: Codename, authPasswd :: Password}
    deriving (Eq, Ord, Show, Read)
type Password = String

data ServerResponse
    = ServerAck
    | ServerError String
    | ServedServerInfo ServerInfo
    | ServedLock Lock
    | ServedUserInfo UserInfo
    | ServedSolution Solution
    | ServedHint Hint
    | ServerCodenameFree
    deriving (Eq, Ord, Show, Read)

data ServerInfo = ServerInfo {serverLockSize :: Int, serverInfoString::String}
    deriving (Eq, Ord, Show, Read)
defaultServerInfo = ServerInfo 8 ""

instance Binary ClientRequest where
    put (ClientRequest pv mauth act) = putPackedInt pv >> put mauth >> put act
    get = liftM3 ClientRequest getPackedInt get get

instance Binary Action where
    put Authenticate = put (0::Word8)
    put Register = put (1::Word8)
    put GetServerInfo = put (2::Word8)
    put (GetLock lspec) = put (3::Word8) >> put lspec
    put (GetUserInfo name) = put (4::Word8) >> put name
    put (GetHint lspec) = put (5::Word8) >> put lspec
    put (GetSolution lspec) = put (6::Word8) >> put lspec
    put (DeclareSolution soln lspec alock idx) = put (7::Word8) >> put soln >> put lspec >> put alock >> put idx
    put (SetLock lock li soln) = put (8::Word8) >> put lock >> put li >> put soln
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> return Authenticate
	    1 -> return Register
	    2 -> return GetServerInfo
	    3 -> liftM GetLock get
	    4 -> liftM GetUserInfo get
	    5 -> liftM GetHint get
	    6 -> liftM GetSolution get
	    7 -> liftM4 DeclareSolution get get get get
	    8 -> liftM3 SetLock get get get

instance Binary Auth where
    put (Auth name pw) = put name >> put pw
    get = liftM2 Auth get get

instance Binary ServerResponse where
    put ServerAck = put (0::Word8)
    put (ServerError error) = put (1::Word8) >> put error
    put (ServedServerInfo sinfo) = put (2::Word8) >> put sinfo
    put (ServedLock lock) = put (3::Word8) >> put lock
    put (ServedUserInfo info) = put (4::Word8) >> put info
    put (ServedSolution soln) = put (5::Word8) >> put soln
    put (ServedHint hint) = put (6::Word8) >> put hint
    put (ServerCodenameFree) = put (7::Word8)
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> return ServerAck
	    1 -> liftM ServerError get
	    2 -> liftM ServedServerInfo get
	    3 -> liftM ServedLock get
	    4 -> liftM ServedUserInfo get
	    5 -> liftM ServedSolution get
	    6 -> liftM ServedHint get
	    7 -> return ServerCodenameFree
instance Binary ServerInfo where
    put (ServerInfo sz str) = put sz >> put str
    get = liftM2 ServerInfo get get
