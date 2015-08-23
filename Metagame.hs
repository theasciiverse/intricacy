-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Metagame where

import Control.Applicative
import Data.Array
import Data.Binary
import Control.Monad
import Data.List (delete)
import Data.Char
import Data.Maybe

import GameStateTypes
import Lock

notesNeeded = 3
maxLocks = 3

type Codename = String

validCodeName name = length name == 3 && all validChar name
    where validChar c = isAscii c && isPrint c && not (isLower c) && c /= ' '

data UserInfo = UserInfo {codename::Codename, userLocks::Array LockIndex (Maybe LockInfo), notesRead::[NoteInfo]}
    deriving (Eq, Ord, Show, Read)

initUserInfo name = UserInfo name (array (0,maxLocks-1) $ zip [0..maxLocks-1] (repeat Nothing)) []

data LockInfo = LockInfo {lockSpec::LockSpec, public::Bool, notesSecured::[NoteInfo], lockSolutions::[NoteInfo], accessedBy::[Codename]}
    deriving (Eq, Ord, Show, Read)

initLockInfo ls = LockInfo ls False [] [] []

data NoteInfo = NoteInfo {noteAuthor::Codename, noteBehind::Maybe ActiveLock, noteOn::ActiveLock}
    deriving (Eq, Ord, Show, Read)

data ActiveLock = ActiveLock {lockOwner::Codename, lockIndex :: LockIndex}
    deriving (Eq, Ord, Show, Read)

data AccessedReason = AccessedPrivyRead | AccessedPrivySolved {noteReadByOwner::Bool} | AccessedEmpty | AccessedPub
    deriving (Eq, Ord, Show, Read)
winsPoint :: AccessedReason -> Bool
winsPoint (AccessedPrivySolved True) = False
winsPoint _ = True

getAccessInfo :: UserInfo -> UserInfo -> [Maybe AccessedReason]
getAccessInfo accessedUInfo accessorUInfo =
    let accessor = codename accessorUInfo
	mlinfos = elems $ userLocks accessedUInfo
	accessedSlot = maybe accessedAllExisting accessedLock
	accessedAllExisting = all (maybe True accessedLock) mlinfos
	accessedLock linfo = public linfo || accessor `elem` accessedBy linfo
    in map (maybe
	    (if accessedAllExisting then Just AccessedEmpty else Nothing)
	    (\linfo -> if public linfo then Just AccessedPub else
		if not $ accessedLock linfo then Nothing else Just $
		    if countRead accessorUInfo linfo >= notesNeeded then AccessedPrivyRead
			else AccessedPrivySolved $ not.null $
			    [ n
			    | n <- lockSolutions linfo 
			    , noteAuthor n == accessor
			    , noteBehind n == Nothing || n `elem` notesRead accessedUInfo ]))
	mlinfos

countRead :: UserInfo -> LockInfo -> Int
countRead reader tlock = fromIntegral $ length
	$ filter (\n -> (isNothing (noteBehind n) || n `elem` notesRead reader)
	    && noteAuthor n /= codename reader)
	$ lockSolutions tlock

data UserInfoDelta
    = AddRead NoteInfo
    | DelRead NoteInfo
    | PutLock LockSpec LockIndex
    | LockDelta LockIndex LockDelta
    deriving (Eq, Ord, Show, Read)
data LockDelta
    = SetPubNote NoteInfo
    | AddSecured NoteInfo
    | DelSecured NoteInfo
    | AddSolution NoteInfo
    | AddAccessed Codename
    | SetPublic
    deriving (Eq, Ord, Show, Read)

data Undeclared = Undeclared Solution LockSpec ActiveLock
    deriving (Eq, Ord, Show, Read)

-- | permanent serial number of a lock
type LockSpec = Int

-- | which of a user's three locks (0,1, or 2)
type LockIndex = Int

-- | solved state
type Hint = GameState

lockIndexChar :: LockIndex -> Char
lockIndexChar i = toEnum $ i + fromEnum 'A'

charLockIndex c = fromEnum (toUpper c) - fromEnum 'A'

alockStr :: ActiveLock -> String
alockStr (ActiveLock name idx) = name ++ [':',lockIndexChar idx]

applyDeltas :: UserInfo -> [UserInfoDelta] -> UserInfo
applyDeltas = foldr applyDelta

applyDelta :: UserInfoDelta -> UserInfo -> UserInfo
applyDelta (AddRead n) info = info { notesRead = n:(notesRead info) }
applyDelta (DelRead n) info = info { notesRead = delete n (notesRead info) }
applyDelta (PutLock ls li) info = info { userLocks = userLocks info // [(li, Just $ initLockInfo ls)] }
applyDelta (LockDelta li ld) info =
    info { userLocks = userLocks info // [(li, liftM (applyLockDelta ld) (userLocks info ! li))] }
applyLockDelta (SetPubNote n) lockinfo = lockinfo { lockSolutions = map
    (\n' -> if n' == n then n {noteBehind=Nothing} else n') (lockSolutions lockinfo) }
applyLockDelta (AddSecured n) lockinfo = lockinfo { notesSecured = n:(notesSecured lockinfo) }
applyLockDelta (DelSecured n) lockinfo = lockinfo { notesSecured = delete n $ notesSecured lockinfo }
applyLockDelta (AddSolution n) lockinfo = lockinfo { lockSolutions = n:(lockSolutions lockinfo) }
applyLockDelta (AddAccessed name) lockinfo = lockinfo { accessedBy = name:(delete name $ accessedBy lockinfo) }
applyLockDelta SetPublic lockinfo = lockinfo { public = True, lockSolutions = [], accessedBy = []}

instance Binary UserInfo where
    put (UserInfo name locks notes) = put name >> put locks >> put notes
    get = liftM3 UserInfo get get get

instance Binary UserInfoDelta where
    put (AddRead note) = put (0::Word8) >> put note
    put (DelRead note) = put (1::Word8) >> put note
    put (PutLock ls li) = put (2::Word8) >> put ls >> put li
    put (LockDelta li ld) = put (3::Word8) >> put li >> put ld
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> AddRead <$> get
	    1 -> DelRead <$> get
	    2 -> PutLock <$> get <*> get
	    3 -> LockDelta <$> get <*> get

instance Binary LockDelta where
    put (SetPubNote note) = put (0::Word8) >> put note
    put (AddSecured note) = put (1::Word8) >> put note
    put (DelSecured note) = put (2::Word8) >> put note
    put (AddSolution note) = put (3::Word8) >> put note
    put (AddAccessed name) = put (4::Word8) >> put name
    put SetPublic = put (5::Word8)
    get = do
	tag <- get :: Get Word8
	case tag of
	    0 -> SetPubNote <$> get
	    1 -> AddSecured <$> get
	    2 -> DelSecured <$> get
	    3 -> AddSolution <$> get
	    4 -> AddAccessed <$> get
	    5 -> return SetPublic

instance Binary LockInfo where
    put (LockInfo spec pk notes solved accessed) = put spec >> put pk >> put notes >> put solved >> put accessed
    get = liftM5 LockInfo get get get get get

instance Binary NoteInfo where
    put (NoteInfo author behind on) = put author >> put behind >> put on
    get = liftM3 NoteInfo get get get

instance Binary ActiveLock where
    put (ActiveLock owner idx) = put owner >> put idx
    get = liftM2 ActiveLock get get
