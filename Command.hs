module Command where
import Hex
import GameState
import Physics (TorqueDir)
import Metagame

data Command = CmdDir WrHoSel HexDir | CmdRotate TorqueDir | CmdWait
	| CmdMoveTo HexPos
	| CmdToggle
	| CmdTile Tile | CmdPaint Tile | CmdMerge
	| CmdSelect | CmdUnselect | CmdDelete
	| CmdPlay | CmdResetTools
	| CmdUndo | CmdRedo
	| CmdReplayForward Int | CmdReplayBack Int
	| CmdInputChar Char
	| CmdWriteState
	| CmdTutorials
	| CmdSelCodename (Maybe Codename) | CmdBackCodename | CmdHome
	| CmdSolve (Maybe LockIndex) | CmdDeclare (Maybe Undeclared)
	| CmdViewSolution (Maybe NoteInfo)
	| CmdSelectLock | CmdNextLock | CmdPrevLock
	| CmdEdit | CmdPlaceLock (Maybe LockIndex)
	| CmdRegister | CmdAuth
	| CmdRedraw
	| CmdQuit 
	| CmdNone
    deriving (Eq, Ord, Show, Read)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show, Read)
	
describeCommand :: Command -> String
describeCommand _ = "" -- TODO
