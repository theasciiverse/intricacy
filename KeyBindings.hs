module KeyBindings (KeyBindings, bindings, findBinding) where

import Data.Map as Map
import Data.Maybe

import Command
import Hex
import GameState
import UI

type KeyBindings = [ (Char,Command) ]
globalBindings =
	[ ('5', CmdWait)
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

	, ('d', CmdDir WHSHook hu)
	, ('a', CmdDir WHSHook $ neg hu)
	, ('z', CmdDir WHSHook hw)
	, ('e', CmdDir WHSHook $ neg hw)
	, ('w', CmdDir WHSHook hv)
	, ('x', CmdDir WHSHook $ neg hv)
	, ('D', CmdDir WHSWrench hu)
	, ('A', CmdDir WHSWrench $ neg hu)
	, ('Z', CmdDir WHSWrench hw)
	, ('E', CmdDir WHSWrench $ neg hw)
	, ('W', CmdDir WHSWrench hv)
	, ('X', CmdDir WHSWrench $ neg hv)
	--, ('i', CmdDir WHSWrench hu)
	--, ('e', CmdDir WHSWrench $ neg hu)
	--, ('j', CmdDir WHSWrench hw)
	--, ('y', CmdDir WHSWrench $ neg hw)
	--, ('p', CmdDir WHSWrench hv)
	--, ('k', CmdDir WHSWrench $ neg hv)
	, ('q', CmdRotate 1)
	, ('c', CmdRotate $ -1)
	, ('.', CmdWait)
	, ('U', CmdUndo)
	, ('u', CmdUndo)
	, ('R', CmdRedo)
	, (ctrl 'R', CmdRedo)
	, ('Q', CmdQuit)
	, ('P', CmdPlay)
	, ('p', CmdPlay)
	, ('\r', CmdSelect)
	, ('\n', CmdSelect)
	, (' ', CmdSelect)
	, ('-', CmdTile $ ArmTile hu False)
	, ('\\', CmdTile $ ArmTile hv False)
	, ('-', CmdTile $ ArmTile zero False)
	, ('@', CmdTile HookTile)
	, ('\t', CmdToggle)
	, ('M', CmdMerge)
	, ('T', CmdResetTools)
	, ('t', CmdResetTools)
	, ('W', CmdWriteState)
	, ('w', CmdWriteState)
	, (ctrl 'S', CmdWriteState)
	, ('K', CmdDelete)
	, ('o', CmdTile $ PivotTile zero)
	, ('O', CmdTile BallTile)
	, ('S', CmdTile $ SpringTile Relaxed zero)
	--, ('s', CmdTile $ SpringTile Stretched zero)
	--, ('$', CmdTile $ SpringTile Compressed zero)
	, ('B', CmdTile $ BlockTile [])
	, ('b', CmdTile $ BlockTile [])
	, ('#', CmdTile $ BlockTile [])
	, ('.', CmdWait)
	]
playOnlyBindings =
	[ ('s', CmdWait)
	, ('S', CmdWait)
	]
editOnlyBindings = []

ctrl :: Char -> Char
ctrl c = toEnum $ fromEnum c - 64

playBindings :: KeyBindings
playBindings = globalBindings ++ playOnlyBindings
editBindings = globalBindings ++ editOnlyBindings
metaBindings =
    [ ('c', CmdSelCodename)
    , ('R', CmdRegister)
    , ('A', CmdAuth)
    , ('q', CmdQuit)
    , ('Q', CmdQuit)
    ]

bindings IMEdit = editBindings
bindings IMPlay = playBindings
bindings IMMeta = metaBindings

findBinding :: KeyBindings -> Command -> String
findBinding bdgs cmd =
    fromMaybe "" $ listToMaybe [ [ch] | (ch,cmd') <- bdgs, cmd'==cmd ]
