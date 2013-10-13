module Command where
import Hex
import GameState
import Physics (TorqueDir)

data Command = CmdDir WrHoSel HexDir | CmdRotate TorqueDir | CmdWait
	| CmdMoveTo HexPos
	| CmdToggle
	| CmdTile Tile | CmdPaint Tile | CmdMerge
	| CmdSelect | CmdUnselect | CmdDelete
	| CmdPlay | CmdResetTools
	| CmdUndo | CmdRedo
	| CmdInputChar Char
	| CmdWriteState
	| CmdTutorials
	| CmdSelCodename | CmdBackCodename
	| CmdSolve | CmdDeclare 
	| CmdSelectLock | CmdNextLock | CmdPrevLock
	| CmdEdit | CmdPlaceLock
	| CmdRegister | CmdAuth
	| CmdRedraw
	| CmdQuit 
    deriving (Eq, Ord, Show, Read)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show, Read)
	
