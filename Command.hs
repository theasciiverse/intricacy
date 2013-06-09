module Command where
import Hex
import GameState
import Physics (TorqueDir)

data Command = CmdDir WrHoSel HexDir | CmdRotate TorqueDir | CmdWait
	| CmdMoveTo HexPos
	| CmdToggle
	| CmdTile Tile | CmdMerge
	| CmdSelect | CmdUnselect | CmdDelete
	| CmdEdit | CmdPlay | CmdTest
	| CmdUndo
	| CmdQuit | CmdWriteState
    deriving (Eq, Ord, Show)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show)
	
