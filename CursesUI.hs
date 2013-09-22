{-# LANGUAGE FlexibleInstances #-}
module CursesUI ( UIM ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State

import Hex
import GameState
import Physics
import Command
import KeyBindings
import Mundanities
import UI

type AttrChar = (Char, Curses.Attr)
type ColPair = Int
data Glyph = Glyph Char ColPair Curses.Attr
tileChar :: Tile -> AttrChar
a0 = Curses.attr0
bold = Curses.setBold a0 True
tileChar (BlockTile _) = ('#',a0)
tileChar (PivotTile dir) | dir == zero = ('o',bold)
    | canonDir dir == hu = ('-',bold)
    | canonDir dir == hv = ('\\',bold)
    | canonDir dir == hw = ('/',bold)
tileChar (ArmTile dir principal) = let
    cdir = canonDir dir
    c | cdir == hu = '-'
      | cdir == hv = '\\'
      | cdir == hw = '/'
    a = if principal then bold else a0
    in (c,a)
tileChar HookTile = ('@',bold)
tileChar (WrenchTile mom) = ('*',if mom /= zero then bold else a0)
tileChar BallTile = ('O',a0)
tileChar (SpringTile Relaxed _) = ('S',a0)
tileChar (SpringTile Compressed _) = ('$',bold)
tileChar (SpringTile Stretched _) = ('s',bold)

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring reversed (owner,t) =
    let (ch,attr) = tileChar t
	pair = case Map.lookup owner colouring of
		    Nothing -> 0
		    Just n -> n+1
	reverse = owner `elem` reversed
    in Glyph ch pair (Curses.setReverse attr reverse)

data CVec = CVec { cy, cx :: Int }
    deriving (Eq, Ord, Show)
instance Monoid CVec where
    mempty = CVec 0 0
    mappend (CVec y x) (CVec y' x') = CVec (y+y') (x+x')
instance Grp CVec where
    neg (CVec y x) = CVec (-y) (-x) 
type CCoord = PHS CVec

hexVec2CVec :: HexVec -> CVec
hexVec2CVec (HexVec x y z) = CVec (-y) (x-z) 

addCh :: Char ->  IO ()
addCh c = Curses.wAddStr Curses.stdScr [c]
mvAddCh :: CVec -> Char ->  IO ()
mvAddCh (CVec y x) c = Curses.mvAddCh y x $ fromIntegral $ ord c
mvAddGlyph :: [Curses.Pair] -> CVec -> Glyph -> IO ()
mvAddGlyph cpairs v (Glyph ch col attr) =
    Curses.attrSet attr (cpairs!!col) >> mvAddCh v ch
move :: CVec -> IO ()
move (CVec y x) = Curses.move y x

data UIState = UIState { dispCPairs::[Curses.Pair], dispCentre::HexPos
    , dispLastCol::PieceColouring, uiKeyBindings :: Map InputMode KeyBindings }
type UIM = StateT UIState IO
nullUIState = UIState [] (PHS zero) Map.empty Map.empty

readBindings :: UIM ()
readBindings = do 
    path <- liftIO $ confFilePath "bindings"
    mbdgs <- liftIO $ readReadFile path
    case mbdgs of
	Just bdgs -> modify $ \s -> s {uiKeyBindings = bdgs}
	Nothing -> return ()

erase :: UIM ()
erase = liftIO Curses.erase
refresh :: UIM ()
refresh = liftIO Curses.refresh

drawAt :: Glyph -> HexPos -> UIM ()
drawAt gl pos = do
	scrCentre <- getScrCentre
	cpairs <- gets dispCPairs
	centre <- gets dispCentre
	let cpos = scrCentre <+> (hexVec2CVec $ pos <-> centre)
	liftIO $ mvAddGlyph cpairs cpos gl

drawCursorAt :: Maybe HexPos -> UIM ()
drawCursorAt Nothing =
    liftIO $ Curses.cursSet Curses.CursorInvisible >> return ()
drawCursorAt (Just pos) = do 
    scrCentre <- getScrCentre
    centre <- gets dispCentre
    liftIO $ Curses.cursSet Curses.CursorVisible
    liftIO $ move $ scrCentre <+> (hexVec2CVec $ pos <-> centre)
    refresh

getScrCentre :: UIM CVec
getScrCentre = do 
    (h,w) <- liftIO Curses.scrSize
    return $ CVec (h`div`2) (w`div`2)

-- TODO: monochrome mode
drawState :: [PieceIdx] -> Bool -> [Alert] -> GameState -> UIM ()
drawState reversed colourFixed alerts st =
    let board = stateBoard st
    in do
	lastCol <- gets dispLastCol
	let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
	modify $ \ds -> ds { dispLastCol = colouring }
	erase
	sequence_ [ drawAt glyph pos | 
	    (pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring reversed) board
	    ]
	refresh

-- From Curses.CursesHelper:
-- | Converts a list of 'Curses.Color' pairs (foreground color and
--   background color) into the curses representation 'Curses.Pair'.
colorsToPairs :: [(Curses.Color, Curses.Color)] -> IO [Curses.Pair]
colorsToPairs cs =
    do p <- Curses.colorPairs
       let nColors = length cs
           blackWhite = p < nColors
       if blackWhite then do
	    print ("Terminal does not support enough colors. Number of " ++
                      " colors requested: " ++ show nColors ++
                      ". Number of colors supported: " ++ show p)
	    return $ take nColors (repeat (Curses.Pair 0))
          else mapM toPairs (zip [1..] cs)
     where toPairs (n, (fg, bg)) =
               let p = Curses.Pair n
               in do Curses.initPair p fg bg
                     return p

say str = do
    (h,w) <- liftIO Curses.scrSize
    liftIO $ Curses.mvWAddStr Curses.stdScr (h-1) 0 str
    refresh

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawPlayState (PlayState (st:_) _ alerts wsel _ solved) = do
	do
	    drawState [] False alerts st
	    when solved $ say "Unlocked!"
	    drawCursorAt $ listToMaybe [ pos |
		(_, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]

    drawEditState (EditState (st:_) _ selPiece selPos _ _) =
	do
	    drawState (maybeToList selPiece) True [] st
	    drawCursorAt $ if isNothing selPiece then Just selPos else Nothing

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert (AlertCollision pos) = drawAt cGlyph pos
	    drawAlert _ = return ()
	    cGlyph = Glyph '!' 0 a0

    askYesNo prompt = say prompt

    initUI = 
	do liftIO $ CursesH.start 
	   cpairs <- liftIO $ colorsToPairs [ (f, CursesH.black) | f <-
		[ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue, CursesH.magenta] ] 
	   modify $ \s -> s {dispCPairs = cpairs}
	   readBindings
	   return True

    endUI = liftIO CursesH.end

    getInput mode = do
	Curses.KeyChar ch <- liftIO $ CursesH.getKey Curses.refresh
	uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
	return $ maybeToList $ lookup ch $ uibdgs ++ bindings mode
