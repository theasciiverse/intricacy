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
    | GetLock LockSpec
    | GetUserInfo Codename
    | GetHint LockSpec
    | GetSolution LockSpec
    | DeclareSolution Solution LockSpec ActiveLock
    | SetLock Lock LockIndex
    deriving (Eq, Ord, Show, Read)

data Auth = Auth Codename Password
    deriving (Eq, Ord, Show, Read)
type Password = String

data ServerResponse
    = ServerAck
    | ServerError String
    | ServedLock Lock
    | ServedUserInfo UserInfo
    | ServedSolution Solution
    | ServedHint Hint
    deriving (Eq, Ord, Show, Read)

instance Binary ClientRequest where
    put (ClientRequest pv mauth act) = putPackedInt pv >> put mauth >> put act
    get = liftM3 ClientRequest getPackedInt get get

instance Binary Action where
    put Authenticate = put (0::Word8)
    put (GetLock lspec) = put (1::Word8) >> put lspec
    put (GetUserInfo name) = put (2::Word8) >> put name
    put (GetHint lspec) = put (3::Word8) >> put lspec
    put (GetSolution lspec) = put (4::Word8) >> put lspec
    put (DeclareSolution soln lspec alock) = put (5::Word8) >> put soln >> put lspec >> put alock
    put (SetLock lock li) = put (6::Word8) >> put lock >> put li
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> return Authenticate
	    1 -> liftM GetLock get
	    2 -> liftM GetUserInfo get
	    3 -> liftM GetHint get
	    4 -> liftM GetSolution get
	    5 -> liftM3 DeclareSolution get get get
	    6 -> liftM2 SetLock get get

instance Binary Auth where
    put (Auth name pw) = put name >> put pw
    get = liftM2 Auth get get

instance Binary ServerResponse where
    put ServerAck = put (0::Word8)
    put (ServerError error) = put (1::Word8) >> put error
    put (ServedLock lock) = put (2::Word8) >> put lock
    put (ServedUserInfo info) = put (3::Word8) >> put info
    put (ServedSolution soln) = put (4::Word8) >> put soln
    put (ServedHint hint) = put (5::Word8) >> put hint
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> return ServerAck
	    1 -> liftM ServerError get
	    2 -> liftM ServedLock get
	    3 -> liftM ServedUserInfo get
	    4 -> liftM ServedSolution get
	    5 -> liftM ServedHint get
