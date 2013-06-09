module Command where
import Hex
import GameState (Tile)
import Physics (TorqueDir)

data Command = CmdDir WrHoSel HexDir | CmdRotate TorqueDir | CmdWait
	| CmdToggle
	| CmdTile Tile | CmdMerge
	| CmdSelect | CmdDelete
	| CmdEdit | CmdPlay | CmdTest
	| CmdUndo
	| CmdQuit | CmdWriteState
    deriving (Eq, Ord, Show)
data WrHoSel = WHSWrench | WHSHook | WHSSelected
    deriving (Eq, Ord, Show)
	
