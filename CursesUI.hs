-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module CursesUI where

import qualified UI.HSCurses.Curses as Curses
import Control.Concurrent.STM
import Control.Applicative
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
import Data.Maybe
import Data.List
import Control.Monad.Trans.Maybe
import Control.Monad.State
import Data.Function (on)

import Hex
import GameState (stateBoard)
import GameStateTypes
import BoardColouring
import Frame
import KeyBindings
import Command
import Mundanities
import ServerAddr
import InputMode
import CursesRender
import CVec

data UIState = UIState
    { dispCPairs::[Curses.Pair]
    , dispCentre::HexPos
    , dispLastCol::PieceColouring
    , uiKeyBindings :: Map InputMode KeyBindings
    , monochrome::Bool
    , message::Maybe (Curses.Attr, ColPair, String)}
type UIM = StateT UIState IO
nullUIState = UIState [] (PHS zero) Map.empty Map.empty False Nothing

readBindings :: UIM ()
readBindings = void.runMaybeT $ do
    path <- liftIO $ confFilePath "bindings"
    bdgs <- MaybeT $ liftIO $ readReadFile path
    lift $ modify $ \s -> s {uiKeyBindings = bdgs}
writeBindings :: UIM ()
writeBindings = do
    path <- liftIO $ confFilePath "bindings"
    bdgs <- gets uiKeyBindings
    liftIO makeConfDir
    liftIO $ writeFile path $ show bdgs

getBindings mode = do
    uibdgs <- Map.findWithDefault [] mode <$> gets uiKeyBindings
    return $ uibdgs ++ bindings mode

bindingsStr :: InputMode -> [Command] -> UIM String
bindingsStr mode cmds = do
    bdgs <- getBindings mode
    return $ (("["++).(++"]")) $ intercalate "," $
	map (maybe "" showKey . findBinding bdgs) cmds


erase :: UIM ()
erase = liftIO Curses.erase
refresh :: UIM ()
refresh = liftIO Curses.refresh

type Geom = (CVec, HexPos)
getGeom :: UIM Geom
getGeom = do
    (h,w) <- liftIO Curses.scrSize
    centre <- gets dispCentre
    return (CVec (h`div`2) (w`div`2), centre)

drawAt :: Glyph -> HexPos -> UIM ()
drawAt gl pos =
    drawAtWithGeom gl pos =<< getGeom
drawAtWithGeom gl pos geom@(scrCentre,centre) =
    drawAtCVec gl $ scrCentre <+> (hexVec2CVec $ pos <-> centre)
drawAtCVec :: Glyph -> CVec -> UIM ()
drawAtCVec gl cpos = do
    cpairs <- gets dispCPairs
    liftIO $ mvAddGlyph cpairs cpos gl

drawStr :: Curses.Attr -> ColPair -> CVec -> String -> UIM ()
drawStr attr col v str = do
    cpairs <- gets dispCPairs
    liftIO $ Curses.attrSet attr (cpairs!!col) >> mvAddStr v str
drawStrGrey :: CVec -> String -> UIM ()
drawStrGrey = drawStr a0 0
drawStrCentred attr col v str =
    drawStr attr col (v <+> CVec 0 (-length str `div` 2)) str

drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt Nothing =
    void $ liftIO $ Curses.cursSet Curses.CursorInvisible
drawCursorAt (Just pos) = do
    geom@(scrCentre,centre) <- getGeom
    liftIO $ Curses.cursSet Curses.CursorVisible
    liftIO $ move $ scrCentre <+> (hexVec2CVec $ pos <-> centre)

drawState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawState reversed colourFixed alerts st = do
    lastCol <- gets dispLastCol
    colouring <- drawStateWithGeom reversed colourFixed lastCol st =<< getGeom
    modify $ \ds -> ds { dispLastCol = colouring }

drawStateWithGeom reversed colourFixed lastCol st geom = do
    let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
    mono <- gets monochrome
    sequence_ [ drawAtWithGeom glyph pos geom |
	(pos,glyph) <- Map.toList $ fmap (ownedTileGlyph mono colouring reversed) $ stateBoard st
	]
    return colouring

drawMsgLine = void.runMaybeT $ do
    (attr,col,str) <- MaybeT $ gets message
    lift $ do
	(h,w) <- liftIO Curses.scrSize
	liftIO $ clearLine $ h-1
	let str' = take (w-1) str
	drawStr attr col (CVec (h-1) 0) str'
setMsgLine attr col str = do
    modify $ \s -> s { message = Just (attr,col,str) }
    drawMsgLine
    refresh

drawTitle (Just title) = do
    (h,w) <- liftIO Curses.scrSize
    drawStrCentred a0 white (CVec 0 (w`div`2)) title
drawTitle Nothing = return ()

say = setMsgLine bold white
sayError = setMsgLine bold red
