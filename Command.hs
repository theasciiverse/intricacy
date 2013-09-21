module Command where
import Hex
import GameState
import Physics (TorqueDir)

data Command = CmdDir WrHoSel HexDir | CmdRotate TorqueDir | CmdWait
	| CmdMoveTo HexPos
	| CmdToggle
	| CmdTile Tile | CmdPaint Tile | CmdMerge
	| CmdSelect | CmdUnselect | CmdDelete
	| CmdEdit | CmdPlay | CmdResetTools
	| CmdUndo | CmdRedo
	| CmdYes | CmdNo
	| CmdRedraw
	| CmdQuit | CmdWriteState
    deriving (Eq, Ord, Show, Read)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show, Read)
	
