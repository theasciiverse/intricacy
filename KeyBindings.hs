module KeyBindings (bindings) where

import Data.Map as Map
import Command
import Hex
import GameState

type KeyBindings = Map Char Command
dvorakBindingsGlobal = Map.fromList [
	('s', CmdDir WHSHook hu)
	, ('h', CmdDir WHSHook $ neg hu)
	, ('b', CmdDir WHSHook hw)
	, ('c', CmdDir WHSHook $ neg hw)
	, ('g', CmdDir WHSHook hv)
	, ('m', CmdDir WHSHook $ neg hv)
	, ('i', CmdDir WHSWrench hu)
	, ('e', CmdDir WHSWrench $ neg hu)
	, ('j', CmdDir WHSWrench hw)
	, ('y', CmdDir WHSWrench $ neg hw)
	, ('p', CmdDir WHSWrench hv)
	, ('k', CmdDir WHSWrench $ neg hv)
	, ('6', CmdDir WHSSelected hu)
	, ('4', CmdDir WHSSelected $ neg hu)
	, ('1', CmdDir WHSSelected hw)
	, ('9', CmdDir WHSSelected $ neg hw)
	, ('7', CmdDir WHSSelected hv)
	, ('3', CmdDir WHSSelected $ neg hv)
	, ('r', CmdRotate 1)
	, ('l', CmdRotate $ -1)
	, ('n', CmdRotate 1)
	, ('t', CmdRotate $ -1)
	, ('8', CmdRotate 1)
	, ('2', CmdRotate $ -1)
	, ('5', CmdWait)
	, ('.', CmdWait)
	, ('u', CmdUndo)
	, ('q', CmdQuit)
	, ('E', CmdEdit)
	, ('P', CmdPlay)
	, ('\r', CmdSelect)
	, ('\n', CmdSelect)
	, (' ', CmdSelect)
	, ('*', CmdTile $ WrenchTile zero)
	, ('@', CmdTile HookTile)
	, ('-', CmdTile $ ArmTile hu False)
	, ('\\', CmdTile $ ArmTile hv False)
	, ('/', CmdTile $ ArmTile hw False)
	]

dvorakBindingsPlayOnly = Map.fromList [
	('\t', CmdToggle)
	, ('S', CmdDir WHSWrench hu)
	, ('H', CmdDir WHSWrench $ neg hu)
	, ('B', CmdDir WHSWrench hw)
	, ('C', CmdDir WHSWrench $ neg hw)
	, ('G', CmdDir WHSWrench hv)
	, ('M', CmdDir WHSWrench $ neg hv)
	]
dvorakBindingsEditOnly = Map.fromList [
	('M', CmdMerge)
	, ('T', CmdTest)
	, ('W', CmdWriteState)
	, ('D', CmdDelete)
	, ('o', CmdTile $ PivotTile zero)
	, ('O', CmdTile $ BallTile False)
	, ('S', CmdTile $ SpringTile Relaxed zero)
	, ('s', CmdTile $ SpringTile Stretched zero)
	, ('$', CmdTile $ SpringTile Compressed zero)
	, ('#', CmdTile $ BlockTile [])
	, ('w', CmdTile $ BlockTile [])
	]

playBindings :: KeyBindings
playBindings = dvorakBindingsGlobal `Map.union` dvorakBindingsPlayOnly
editBindings :: KeyBindings
editBindings = dvorakBindingsGlobal `Map.union` dvorakBindingsEditOnly

bindings GMEdit = editBindings
bindings (GMPlay _) = playBindings
