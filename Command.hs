module Command where
import Hex
import GameStateTypes
import Metagame

data Command = CmdDir WrHoSel HexDir | CmdRotate TorqueDir | CmdWait
	| CmdMoveTo HexPos
	| CmdToggle
	| CmdOpen
	| CmdTile Tile | CmdPaint (Maybe Tile)
	| CmdPaintFromTo (Maybe Tile) HexPos HexPos
	| CmdSelect | CmdUnselect
	| CmdDelete | CmdMerge
	| CmdMark | CmdJumpMark
	| CmdPlay | CmdResetTools
	| CmdUndo | CmdRedo
	| CmdReplayForward Int | CmdReplayBack Int
	| CmdInputChar Char
	| CmdWriteState
	| CmdTutorials
	| CmdSetServer | CmdToggleCacheOnly
	| CmdSelCodename (Maybe Codename) | CmdBackCodename | CmdHome
	| CmdSolve (Maybe LockIndex) | CmdDeclare (Maybe Undeclared)
	| CmdViewSolution (Maybe NoteInfo)
	| CmdSelectLock | CmdNextLock | CmdPrevLock
	| CmdEdit | CmdPlaceLock (Maybe LockIndex)
	| CmdRegister | CmdAuth
	| CmdNextPage | CmdPrevPage
	| CmdRedraw
	| CmdQuit 
	| CmdHelp
	| CmdNone
    deriving (Eq, Ord, Show, Read)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show, Read)
	
describeCommand :: Command -> String
describeCommand (CmdDir whs dir) = "Move " ++ whsStr whs ++ " " ++ dirStr dir
describeCommand (CmdRotate dir) = "Rotate " ++ (if dir == 1 then "counter" else "") ++ "clockwise"
describeCommand CmdWait = "nothing"
describeCommand CmdToggle = "toggle tool"
describeCommand CmdOpen = "open lock"
describeCommand (CmdTile tile) = tileStr tile
describeCommand CmdMerge = "merge adjacent blocks" 
describeCommand CmdMark = "mark state"
describeCommand CmdJumpMark = "jump to marked state"
describeCommand CmdSelect = "select piece" 
describeCommand CmdUnselect = "unselect piece"
describeCommand CmdDelete = "delete piece"
describeCommand CmdPlay = "play lock"
describeCommand CmdResetTools = "reset tools"
describeCommand CmdUndo = "undo"
describeCommand CmdRedo = "redo"
describeCommand (CmdReplayForward _) = "advance replay"
describeCommand (CmdReplayBack _) = "rewind replay"
describeCommand CmdWriteState = "write lock"
describeCommand CmdTutorials = "play tutorial levels"
describeCommand CmdSetServer = "set server"
describeCommand CmdToggleCacheOnly = "toggle using only cache"
describeCommand (CmdSelCodename _) = "select player"
describeCommand CmdBackCodename = "select last player"
describeCommand CmdHome = "select self"
describeCommand (CmdSolve _) = "solve lock"
describeCommand (CmdDeclare _) = "declare solution"
describeCommand (CmdViewSolution _) = "view lock solution"
describeCommand CmdSelectLock = "choose lock"
describeCommand CmdNextLock = "next lock"
describeCommand CmdPrevLock = "previous lock"
describeCommand CmdNextPage = "page forward through lists"
describeCommand CmdPrevPage = "page back through lists"
describeCommand CmdEdit = "edit lock"
describeCommand (CmdPlaceLock _) = "place lock"
describeCommand CmdRegister = "register codename"
describeCommand CmdAuth = "authenticate"
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
