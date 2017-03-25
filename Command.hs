-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Command where
import Hex
import GameStateTypes
import Metagame

data Command
    = CmdDir WrHoSel HexDir | CmdRotate WrHoSel TorqueDir | CmdWait
    | CmdMoveTo HexPos | CmdManipulateToolAt HexPos
    | CmdDrag HexPos HexDir
    | CmdToggle
    | CmdOpen
    | CmdTile Tile | CmdPaint (Maybe Tile)
    | CmdPaintFromTo (Maybe Tile) HexPos HexPos
    | CmdSelect | CmdUnselect
    | CmdDelete | CmdMerge
    | CmdMark | CmdJumpMark | CmdReset
    | CmdPlay | CmdTest
    | CmdUndo | CmdRedo
    | CmdReplayForward Int | CmdReplayBack Int
    | CmdInputChar Char
    | CmdInputSelLock LockIndex
    | CmdInputCodename Codename
    | CmdInputSelUndecl Undeclared
    | CmdWriteState
    | CmdTutorials
    | CmdShowRetired | CmdPlayLockSpec (Maybe LockSpec)
    | CmdSetServer | CmdToggleCacheOnly
    | CmdSelCodename (Maybe Codename) | CmdBackCodename | CmdHome
    | CmdSolve (Maybe LockIndex) | CmdDeclare (Maybe Undeclared)
    | CmdViewSolution (Maybe NoteInfo)
    | CmdSelectLock | CmdNextLock | CmdPrevLock
    | CmdEdit | CmdPlaceLock (Maybe LockIndex)
    | CmdRegister Bool | CmdAuth
    | CmdNextPage | CmdPrevPage
    | CmdToggleColourMode
    | CmdRedraw | CmdRefresh | CmdSuspend | CmdClear
    | CmdQuit | CmdForceQuit
    | CmdHelp | CmdBind (Maybe Command)
    | CmdNone
    deriving (Eq, Ord, Show, Read)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show, Read)

describeCommand :: Command -> String
describeCommand (CmdDir whs dir) = "move " ++ whsStr whs ++ " " ++ dirStr dir
describeCommand (CmdRotate whs dir) = "rotate " ++ whsStr whs
	++ " " ++ (if dir == 1 then "counter" else "") ++ "clockwise"
describeCommand CmdWait = "nothing"
describeCommand CmdToggle = "toggle tool"
describeCommand CmdOpen = "open lock"
describeCommand (CmdTile tile) = tileStr tile
describeCommand CmdMerge = "merge with adjacent piece"
describeCommand CmdMark = "mark state"
describeCommand CmdJumpMark = "jump to marked state"
describeCommand CmdReset = "jump to initial state"
describeCommand CmdSelect = "select piece"
describeCommand CmdUnselect = "unselect piece"
describeCommand CmdDelete = "delete piece"
describeCommand CmdPlay = "play lock"
describeCommand CmdTest = "test lock"
describeCommand CmdUndo = "undo"
describeCommand CmdRedo = "redo"
describeCommand (CmdReplayForward _) = "advance replay"
describeCommand (CmdReplayBack _) = "rewind replay"
describeCommand CmdWriteState = "write lock"
describeCommand CmdTutorials = "play tutorial levels"
describeCommand CmdShowRetired = "toggle showing retired locks"
describeCommand CmdSetServer = "set server"
describeCommand CmdToggleCacheOnly = "toggle offline mode"
describeCommand (CmdSelCodename mname) = "select player"
	++ maybe "" (' ':) mname
describeCommand CmdBackCodename = "select last player"
describeCommand CmdHome = "select self"
describeCommand (CmdSolve mli) = "solve lock"
	++ maybe "" ((' ':).(:"").lockIndexChar) mli
describeCommand (CmdPlayLockSpec mls) = "find lock by number"
	++ maybe "" ((' ':).show) mls
describeCommand (CmdDeclare mundecl) = "declare solution"
	++ maybe "" (\_->" [specified solution]") mundecl
describeCommand (CmdViewSolution mnote) = "view lock solution"
	++ maybe "" (\_->" [specified solution]") mnote
describeCommand CmdSelectLock = "choose lock by name"
describeCommand CmdNextLock = "next lock"
describeCommand CmdPrevLock = "previous lock"
describeCommand CmdNextPage = "page forward through lists"
describeCommand CmdPrevPage = "page back through lists"
describeCommand CmdEdit = "edit lock"
describeCommand (CmdPlaceLock mli) = "place lock"
	++ maybe "" ((' ':).(:"").lockIndexChar) mli
describeCommand (CmdRegister False) = "register codename"
describeCommand (CmdRegister True) = "adjust registration details"
describeCommand CmdAuth = "authenticate"
describeCommand (CmdBind _) = "bind key"
describeCommand CmdToggleColourMode = "toggle lock colour mode"
describeCommand CmdQuit = "quit"
describeCommand CmdHelp = "help"
describeCommand _ = ""

tileStr HookTile = "hook"
tileStr (WrenchTile _) = "wrench"
tileStr (ArmTile _ _) = "arm"
tileStr (PivotTile _) = "pivot"
tileStr (SpringTile _ _) = "spring"
tileStr (BlockTile _) = "block"
tileStr BallTile = "ball"
whsStr WHSWrench = "wrench"
whsStr WHSHook = "hook"
whsStr WHSSelected = "tool"
dirStr v
    | v == hu = "right"
    | v == neg hu = "left"
    | v == hv = "up-left"
    | v == neg hv = "down-right"
    | v == hw = "down-left"
    | v == neg hw = "up-right"
dirStr _ = ""
