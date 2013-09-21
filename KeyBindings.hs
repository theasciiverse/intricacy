module KeyBindings (KeyBindings, bindings, findBinding) where

import Data.Map as Map
import Data.Maybe

import Command
import Hex
import GameState
import UI

type KeyBindings = [ (Char,Command) ]
dvorakBindingsGlobal = [
	('5', CmdWait)
	, ('6', CmdDir WHSSelected hu)
	, ('4', CmdDir WHSSelected $ neg hu)
	, ('1', CmdDir WHSSelected hw)
	, ('9', CmdDir WHSSelected $ neg hw)
	, ('7', CmdDir WHSSelected hv)
	, ('3', CmdDir WHSSelected $ neg hv)
	, ('8', CmdRotate 1)
	, ('2', CmdRotate $ -1)
	, ('*', CmdTile $ WrenchTile zero)
	, ('/', CmdTile $ ArmTile hw False)

	, ('s', CmdDir WHSHook hu)
	, ('h', CmdDir WHSHook $ neg hu)
	, ('b', CmdDir WHSHook hw)
	, ('c', CmdDir WHSHook $ neg hw)
	, ('g', CmdDir WHSHook hv)
	, ('m', CmdDir WHSHook $ neg hv)
	--, ('i', CmdDir WHSWrench hu)
	--, ('e', CmdDir WHSWrench $ neg hu)
	--, ('j', CmdDir WHSWrench hw)
	--, ('y', CmdDir WHSWrench $ neg hw)
	--, ('p', CmdDir WHSWrench hv)
	--, ('k', CmdDir WHSWrench $ neg hv)
	, ('r', CmdRotate 1)
	, ('l', CmdRotate $ -1)
	, ('.', CmdWait)
	, ('U', CmdUndo)
	, ('u', CmdUndo)
	, ('R', CmdRedo)
	, ('Q', CmdQuit)
	, ('q', CmdQuit)
	, ('P', CmdPlay)
	, ('p', CmdPlay)
	, ('\r', CmdSelect)
	, ('\n', CmdSelect)
	, (' ', CmdSelect)
	, ('-', CmdTile $ ArmTile hu False)
	, ('\\', CmdTile $ ArmTile hv False)
	, ('@', CmdTile HookTile)
	]

dvorakBindingsPlayOnly = [
	('\t', CmdToggle)
	, ('S', CmdDir WHSWrench hu)
	, ('H', CmdDir WHSWrench $ neg hu)
	, ('B', CmdDir WHSWrench hw)
	, ('C', CmdDir WHSWrench $ neg hw)
	, ('G', CmdDir WHSWrench hv)
	, ('M', CmdDir WHSWrench $ neg hv)
	]
dvorakBindingsEditOnly = [
	('M', CmdMerge)
	, ('T', CmdResetTools)
	, ('t', CmdResetTools)
	, ('W', CmdWriteState)
	, ('w', CmdWriteState)
	, ('D', CmdDelete)
	, ('o', CmdTile $ PivotTile zero)
	, ('O', CmdTile BallTile)
	, ('S', CmdTile $ SpringTile Relaxed zero)
	--, ('s', CmdTile $ SpringTile Stretched zero)
	--, ('$', CmdTile $ SpringTile Compressed zero)
	, ('#', CmdTile $ BlockTile [])
	, ('w', CmdTile $ BlockTile [])
	]

playBindings :: KeyBindings
playBindings = dvorakBindingsGlobal ++ dvorakBindingsPlayOnly
editBindings :: KeyBindings
editBindings = dvorakBindingsGlobal ++ dvorakBindingsEditOnly

bindings IMEdit = editBindings
bindings IMPlay = playBindings
bindings IMConfirm = [
    ('Y', CmdYes), ('N', CmdNo)
    , ('y', CmdYes), ('n', CmdNo) ]

findBinding :: KeyBindings -> Command -> String
findBinding bdgs cmd =
    fromMaybe "" $ listToMaybe [ [ch] | (ch,cmd') <- bdgs, cmd'==cmd ]
