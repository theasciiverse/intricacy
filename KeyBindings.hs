-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module KeyBindings (KeyBindings, bindings, findBindings, findBinding, showKey, showKeyFriendly) where

import Data.Maybe
import Data.Char
import Data.List

import Command
import Hex
import GameStateTypes
import InputMode

type KeyBindings = [ (Char,Command) ]

ctrl, unctrl :: Char -> Char
ctrl c = toEnum $ fromEnum c - 64
unctrl c = toEnum $ fromEnum c + 64

lowerToo :: KeyBindings -> KeyBindings
lowerToo = concat . map addLower
    where addLower b@(c, cmd) = [ b, (toLower c, cmd) ]

qwertyViHex =
    [ ('l', CmdDir WHSHook hu)
    , ('h', CmdDir WHSHook $ neg hu)
    , ('b', CmdDir WHSHook hw)
    , ('u', CmdDir WHSHook $ neg hw)
    , ('y', CmdDir WHSHook hv)
    , ('n', CmdDir WHSHook $ neg hv)
    , ('L', CmdDir WHSWrench hu)
    , ('H', CmdDir WHSWrench $ neg hu)
    , ('B', CmdDir WHSWrench hw)
    , ('U', CmdDir WHSWrench $ neg hw)
    , ('Y', CmdDir WHSWrench hv)
    , ('N', CmdDir WHSWrench $ neg hv)
    , ('k', CmdRotate 1)
    , ('j', CmdRotate $ -1)
    ]
qwertyLeftHex =
    [ ('d', CmdDir WHSHook hu)
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
    , ('q', CmdRotate 1)
    , ('c', CmdRotate $ -1)
    , ('S', CmdWait)
    , ('s', CmdWait)
    ]
dvorakMidHex =
    [ ('i', CmdDir WHSWrench hu)
    , ('e', CmdDir WHSWrench $ neg hu)
    , ('j', CmdDir WHSWrench hw)
    , ('y', CmdDir WHSWrench $ neg hw)
    , ('p', CmdDir WHSWrench hv)
    , ('k', CmdDir WHSWrench $ neg hv)
    ]

keypadHex =
    [ ('5', CmdWait)
    , ('6', CmdDir WHSSelected hu)
    , ('4', CmdDir WHSSelected $ neg hu)
    , ('1', CmdDir WHSSelected hw)
    , ('9', CmdDir WHSSelected $ neg hw)
    , ('7', CmdDir WHSSelected hv)
    , ('3', CmdDir WHSSelected $ neg hv)
    , ('8', CmdRotate 1)
    , ('2', CmdRotate $ -1)
    ]

miscLockGlobal = lowerToo
    [ ('X', CmdUndo)
    , ('\b', CmdUndo)
    , ('R', CmdRedo)
    , (ctrl 'R', CmdRedo)
    , (ctrl 'U', CmdUndo)
    , ('.', CmdWait)
    , ('Z', CmdWait)
    , ('M', CmdMark)
    , ('\'', CmdJumpMark)
    ]

miscGlobal = lowerToo
    [ ('Q', CmdQuit)
    , (ctrl 'C', CmdQuit)
    , ('?', CmdHelp)
    , (ctrl 'B', CmdBind)
    , (ctrl 'L', CmdRedraw)
    , (ctrl 'Z', CmdSuspend)
    ]

lockGlobal = keypadHex ++ qwertyViHex ++ miscGlobal ++ miscLockGlobal

playOnly = lowerToo
    [ ('O', CmdOpen)
    , (' ', CmdWait)
    , ('\r', CmdWait)
    , ('\n', CmdWait)
    , ('\t', CmdToggle)
    , ('*', CmdTile $ WrenchTile zero)
    , ('/', CmdTile HookTile)
    , ('@', CmdTile HookTile)
    ]
replayOnly =
    [ (' ', CmdReplayForward 1)
    ]
editMisc = lowerToo
    [ ('P', CmdPlay)
    , (' ', CmdSelect)
    , ('\r', CmdSelect)
    , ('\n', CmdSelect)
    , ('T', CmdTest)
    , ('W', CmdWriteState)
    , (ctrl 'S', CmdWriteState)
    , ('=', CmdMerge)
    , ('+', CmdMerge)
    , ('&', CmdMerge)
    , ('0', CmdDelete)
    , ('E', CmdDelete)
    ]
tilesPaintRow = lowerToo
    [ ('G', CmdTile BallTile)
    , ('F', CmdTile $ ArmTile zero False)
    , ('D', CmdTile $ PivotTile zero)
    , ('S', CmdTile $ SpringTile Relaxed zero)
    , ('A', CmdTile $ BlockTile [])
    , ('Z', CmdDelete)
    ]
tilesAscii =
    [ ('o', CmdTile $ PivotTile zero)
    , ('O', CmdTile BallTile)
    , ('S', CmdTile $ SpringTile Relaxed zero)
    --, ('s', CmdTile $ SpringTile Stretched zero)
    --, ('$', CmdTile $ SpringTile Compressed zero)
    , ('-', CmdTile $ ArmTile zero False)
    --, ('-', CmdTile $ ArmTile hu False)
    , ('\\', CmdTile $ ArmTile hv False)
    , ('/', CmdTile $ ArmTile hw False)
    , ('@', CmdTile HookTile)
    , ('*', CmdTile $ WrenchTile zero)
    , ('#', CmdTile $ BlockTile [])
    ]
editOnly = tilesPaintRow ++ editMisc ++ tilesAscii

playBindings = playOnly ++ lockGlobal
replayBindings = replayOnly ++ lockGlobal
editBindings = editOnly ++ lockGlobal
metaBindings = lowerToo
    [ ('C', CmdSelCodename Nothing)
    , ('H', CmdHome)
    , ('B', CmdBackCodename)
    , ('S', CmdSolve Nothing)
    , ('D', CmdDeclare Nothing)
    , ('V', CmdViewSolution Nothing)
    , ('R', CmdRegister)
    , ('P', CmdPlaceLock Nothing)
    , ('E', CmdEdit)
    , ('L', CmdSelectLock)
    , ('O', CmdPrevLock)
    , ('N', CmdNextLock)
    , ('A', CmdAuth)
    , ('T', CmdTutorials)
    , ('+', CmdShowRetired)
    , ('#', CmdPlayLockSpec Nothing)
    , ('$', CmdSetServer)
    , ('^', CmdToggleCacheOnly)
    , ('>', CmdNextPage)
    , ('<', CmdPrevPage)
    ] ++ miscGlobal

bindings :: InputMode -> KeyBindings
bindings IMEdit = editBindings
bindings IMPlay = playBindings
bindings IMMeta = metaBindings
bindings IMReplay = replayBindings
bindings _ = []

findBindings :: KeyBindings -> Command -> [Char]
findBindings bdgs cmd = nub [ ch | (ch,cmd') <- bdgs, cmd'==cmd ]

findBinding :: KeyBindings -> Command -> Maybe Char
findBinding = (listToMaybe.) . findBindings

showKey ch | isPrint ch = [ch]
showKey ch | isPrint (unctrl ch) = ('^':[unctrl ch])
showKey _ = "[?]"

showKeyFriendly ' ' = "space"
showKeyFriendly '\r' = "return"
showKeyFriendly '\n' = "newline"
showKeyFriendly '\t' = "tab"
showKeyFriendly '\b' = "bksp"
showKeyFriendly ch = showKey ch
