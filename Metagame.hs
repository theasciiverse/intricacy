module Metagame where

import Data.Array
import Data.Binary
import Control.Monad

import GameState
import Lock
import Physics

notesNeeded = 3
maxLocks = 3

type Codename = String

data UserInfo = UserInfo {codename::Codename, userLocks::Array LockIndex (Maybe LockInfo), notesRead::[NoteInfo]}
    deriving (Eq, Ord, Show, Read)

initUserInfo name = UserInfo name (array (0,maxLocks-1) $ zip [0..maxLocks-1] (repeat Nothing)) []

data LockInfo = LockInfo {lockSpec::LockSpec, public::Bool, publicNotes::[NoteInfo], notesSecured::[NoteInfo], lockSolutions::[NoteInfo], accessedBy::[Codename]}
    deriving (Eq, Ord, Show, Read)

initLockInfo ls = LockInfo ls False [] [] [] []

data UserInfoDelta
    = AddRead NoteInfo
    | PutLock LockSpec LockIndex
    | LockDelta LockIndex LockDelta
    deriving (Eq, Ord, Show, Read)
data LockDelta
    = AddPubNote NoteInfo
    | AddSecured NoteInfo
    | AddSolution NoteInfo
    | AddAccessed Codename
    | SetPublic
    deriving (Eq, Ord, Show, Read)

data NoteInfo = NoteInfo {noteAuthor::Codename, noteOn::ActiveLock} 
    deriving (Eq, Ord, Show, Read)

data ActiveLock = ActiveLock {lockOwner::Codename, lockIndex :: LockIndex}
    deriving (Eq, Ord, Show, Read)

-- | permanent serial number of a lock
type LockSpec = Int

-- | which of a user's three locks (0,1, or 2)
type LockIndex = Int

-- | solved state
type Hint = GameState

applyDelta :: UserInfoDelta -> UserInfo -> UserInfo
applyDelta (AddRead n) info = info { notesRead = n:(notesRead info) }
applyDelta (PutLock ls li) info = info { userLocks = userLocks info // [(li, Just $ initLockInfo ls)] }
applyDelta (LockDelta li ld) info =
    info { userLocks = userLocks info // [(li, liftM (applyLockDelta ld) (userLocks info ! li))] }
applyLockDelta (AddPubNote n) lockinfo = lockinfo { publicNotes = n:(publicNotes lockinfo) }
applyLockDelta (AddSecured n) lockinfo = lockinfo { notesSecured = n:(notesSecured lockinfo) }
applyLockDelta (AddSolution n) lockinfo = lockinfo { lockSolutions = n:(lockSolutions lockinfo) }
applyLockDelta (AddAccessed n) lockinfo = lockinfo { accessedBy = n:(accessedBy lockinfo) }
applyLockDelta SetPublic lockinfo = lockinfo { public = True, publicNotes = [], lockSolutions = [], accessedBy = []}

instance Binary UserInfo where
    put (UserInfo name locks notes) = put name >> put locks >> put notes
    get = liftM3 UserInfo get get get

instance Binary LockInfo where
    put (LockInfo spec pub pk notes solved accessed) = put spec >> put pub >> put pk >> put notes >> put solved >> put accessed
    get = liftM6 LockInfo get get get get get get

liftM6 f g1 g2 g3 g4 g5 g6 = do
    nearly <- liftM5 f g1 g2 g3 g4 g5
    liftM nearly g6

instance Binary NoteInfo where
    put (NoteInfo author on) = put author >> put on
    get = liftM2 NoteInfo get get

instance Binary ActiveLock where
    put (ActiveLock owner idx) = put owner >> put idx
    get = liftM2 ActiveLock get get
