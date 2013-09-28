module Metagame where

import Data.Binary
import Control.Monad

import GameState
import Lock
import Physics

type Codename = String

data UserInfo = UserInfo {codename::Codename, locks::[LockInfo], notesRead::[NoteInfo]}
    deriving (Eq, Ord, Show, Read)

data LockInfo = LockInfo {lockSpec::LockSpec, notesSecured::[NoteInfo], solvedBy::[Codename], accessedBy::[Codename]}
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

type Solution = [PlayerMove]


instance Binary UserInfo where
    put (UserInfo name locks notes) = put name >> put locks >> put notes
    get = liftM3 UserInfo get get get

instance Binary LockInfo where
    put (LockInfo spec notes solved accessed) = put spec >> put notes >> put solved >> put accessed
    get = liftM4 LockInfo get get get get

instance Binary NoteInfo where
    put (NoteInfo author on) = put author >> put on
    get = liftM2 NoteInfo get get

instance Binary ActiveLock where
    put (ActiveLock owner idx) = put owner >> put idx
    get = liftM2 ActiveLock get get
