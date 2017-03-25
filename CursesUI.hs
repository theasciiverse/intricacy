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
import Data.Foldable (forM_)

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

getBindings :: InputMode -> UIM KeyBindings
getBindings mode = do
    uibdgs <- Map.findWithDefault [] mode <$> gets uiKeyBindings
    return $ uibdgs ++ bindings mode

bindingsStr :: InputMode -> [Command] -> UIM String
bindingsStr mode cmds = do
    bdgs <- getBindings mode
    return $ (("["++).(++"]")) $ intercalate "," $
	map (maybe "" showKey . findBinding bdgs) cmds


data Gravity = GravUp | GravLeft | GravRight | GravDown | GravCentre
    deriving (Eq, Ord, Show, Enum)

data Draw = Draw { drawWidth :: Int, doDraw :: CVec -> UIM () }

doDrawAt :: CVec -> Draw -> UIM ()
doDrawAt = flip doDraw

alignDraw :: Gravity -> Int -> Draw -> Draw
alignDraw gravity w (Draw w' d) = Draw (max w w') $ \(CVec y x) ->
    d $ CVec y $ x + shift
    where
        shift = case gravity of
            GravLeft -> 0
            GravRight -> max 0 $ w - w'
            _ -> max 0 . (`div` 2) $ w - w'

instance Monoid Draw where
    mempty = Draw 0 (const $ return ())
    mappend (Draw w d) (Draw w' d') =
        Draw (w+w') $ \cpos@(CVec y x) -> d cpos >> d' (CVec y (x+w))

stringDraw :: Curses.Attr -> ColPair -> String -> Draw
stringDraw attr col str =
    Draw (length str) $ \cpos -> drawStr attr col cpos str

greyDraw :: String -> Draw
greyDraw = stringDraw a0 white

bindingsDraw :: KeyBindings -> [Command] -> Draw
bindingsDraw = bindingsDrawColour white
bindingsDrawColour :: ColPair -> KeyBindings -> [Command] -> Draw
bindingsDrawColour col bdgs cmds =
    mconcat . ((stringDraw a0 col "[") :) . (++ [stringDraw a0 col "]"]) .
    intersperse (stringDraw a0 col ",") $
        catMaybes $ ((keyDraw <$>) . findBinding bdgs) <$> cmds
    where
        keyDraw = stringDraw bold col . showKeyFriendlyShort

data BindingsEntry = BindingsEntry String [Command]

drawBindingsTables :: InputMode -> Frame -> UIM ()
drawBindingsTables mode frame | mode `elem` [ IMEdit, IMPlay ] = do
    bdgs <- getBindings mode
    (h,w) <- liftIO Curses.scrSize
    let startRight = frameWidth frame + 3
    let maxWidth = (w `div` 2) - startRight - 1
    let entryDraws (BindingsEntry desc cmds) =
            (greyDraw desc, bindingsDraw bdgs cmds)
    forM_ [GravLeft, GravRight] $ \grav ->
        let table = bindingsTable mode grav
            drawsTable = map (\(line, entry) -> (line, entryDraws entry)) table
            maxDesc = maximum $ map (drawWidth . fst . snd) drawsTable
            maxBdgs = maximum $ map (drawWidth . snd . snd) drawsTable
            descX = (w `div` 2) + if grav == GravRight
                then startRight + maxBdgs + 2
                else -(startRight + maxBdgs + 2 + maxDesc)
            bdgsX = (w `div` 2) + if grav == GravRight
                then startRight
                else -(startRight + maxBdgs)
            oppGrav = if grav == GravRight then GravLeft else GravRight
            useDescs = maxDesc + 1 + maxBdgs <= maxWidth
        in sequence_
            [ do
                when (maxBdgs <= maxWidth) $
                    doDrawAt (CVec y bdgsX) $ alignDraw
                        (if useDescs then grav else oppGrav) maxBdgs
                        bdgsDraw
                when useDescs $
                    doDrawAt (CVec y descX) $ alignDraw oppGrav maxDesc descDraw
            | (yoff, (descDraw, bdgsDraw)) <- drawsTable
            , let y = (h `div` 2) + yoff
            ]
    where
        bindingsTable IMPlay GravLeft =
            [ (-5, BindingsEntry "move tool" $
                map (CmdDir WHSSelected) hexDirs)
            , (-4, BindingsEntry "select tool"
                [CmdToggle, CmdTile $ WrenchTile zero, CmdTile HookTile])
            , (-3, BindingsEntry "move hook" $
                map (CmdDir WHSHook) hexDirs)
            , (-2, BindingsEntry "move wrench" $
                map (CmdDir WHSWrench) hexDirs)
            , (-1, BindingsEntry "rotate hook"
                [CmdRotate whs dir | whs <- [WHSSelected, WHSHook], dir <- [-1,1]])
            , ( 0, BindingsEntry "wait" [CmdWait])
            , ( 2, BindingsEntry "open lock" [CmdOpen])
            , ( 4, BindingsEntry "undo, redo" [CmdUndo, CmdRedo])
            , ( 5, BindingsEntry "marks" [CmdMark, CmdJumpMark, CmdReset])
            ]
        bindingsTable IMPlay GravRight =
            [ (-7, BindingsEntry "help" [CmdHelp])
            , (7, BindingsEntry "quit" [CmdQuit])
            ]
        bindingsTable IMEdit GravLeft =
            [ (-4, BindingsEntry "move" $ map (CmdDir WHSSelected) hexDirs)
            , (-3, BindingsEntry "rotate"
                [CmdRotate whs dir | whs <- [WHSSelected], dir <- [-1,1]])
            , (-1, BindingsEntry "select" [CmdSelect])
            , ( 0, BindingsEntry "delete" [CmdDelete])
            , ( 1, BindingsEntry "merge" [CmdMerge])
            , ( 4, BindingsEntry "undo, redo" [CmdUndo, CmdRedo])
            , ( 5, BindingsEntry "marks" [CmdMark, CmdJumpMark, CmdReset])
            ]
        bindingsTable IMEdit GravRight =
            [ (-7, BindingsEntry "help" [CmdHelp])
            , (-4, BindingsEntry "place" $ map CmdTile
                [ BlockTile []
                , SpringTile Relaxed zero
                , PivotTile zero
                , ArmTile zero False
                , BallTile
                ])
            , (-1, BindingsEntry "test" [CmdTest])
            , ( 0, BindingsEntry "play" [CmdPlay])
            , ( 1, BindingsEntry "step" [CmdWait])
            , ( 4, BindingsEntry "write" [CmdWriteState])
            , ( 7, BindingsEntry "quit" [CmdQuit])
            ]
        bindingsTable _ _ = []

drawBindingsTables _ _ = return ()

-- |frameWidth = maximum . map (abs . cx . hexVec2CVec) .
--      blockPattern . placedPiece . framePiece
frameWidth :: Frame -> Int
frameWidth frame@(BasicFrame size) = max (2*size) $
    2*(size + boltWidth frame) - (size`div`2)

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

drawAtWithGeom :: Glyph -> HexPos -> Geom -> UIM ()
drawAtWithGeom gl pos geom@(scrCentre,centre) =
    drawAtCVec gl $ scrCentre +^ (hexVec2CVec $ pos -^ centre)

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
drawStrCentred :: Curses.Attr -> ColPair -> CVec -> [Char] -> UIM ()
drawStrCentred attr col v str =
    drawStr attr col (truncateCVec $ (v +^ CVec 0 (-length str `div` 2))) str


drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt Nothing =
    void $ liftIO $ Curses.cursSet Curses.CursorInvisible
drawCursorAt (Just pos) = do
    geom@(scrCentre,centre) <- getGeom
    liftIO $ Curses.cursSet Curses.CursorVisible
    liftIO $ move $ scrCentre +^ (hexVec2CVec $ pos -^ centre)

drawState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawState reversed colourFixed alerts st = do
    lastCol <- gets dispLastCol
    colouring <- drawStateWithGeom reversed colourFixed lastCol st =<< getGeom
    modify $ \ds -> ds { dispLastCol = colouring }

drawStateWithGeom :: [PieceIdx] -> Bool -> PieceColouring -> GameState -> Geom -> UIM PieceColouring
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
