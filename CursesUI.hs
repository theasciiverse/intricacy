{-# LANGUAGE FlexibleInstances #-}
module CursesUI ( UIM ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Concurrent.STM
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
import ServerAddr
import Cache
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
mvAddStr :: CVec -> String -> IO ()
mvAddStr (CVec y x) str = Curses.mvWAddStr Curses.stdScr y x str
mvAddGlyph :: [Curses.Pair] -> CVec -> Glyph -> IO ()
mvAddGlyph cpairs v (Glyph ch col attr) =
    Curses.attrSet attr (cpairs!!col) >> mvAddCh v ch
move :: CVec -> IO ()
move (CVec y x) = Curses.move y x
clearLine :: Int -> IO ()
clearLine y = Curses.move y 0 >> Curses.clrToEol

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

red = 1

drawMsgLine attr col str = do
    (h,w) <- liftIO Curses.scrSize
    liftIO $ clearLine $ h-1
    drawStr attr col (CVec (h-1) 0) str
    refresh
say = drawMsgLine bold 0
sayError = drawMsgLine bold 1

instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState
    drawMainState (PlayState st _ alerts wsel solved _ _) = do
	drawState [] False alerts st
	when solved $ say "Unlocked!"
	drawCursorAt $ listToMaybe [ pos |
	    (_, PlacedPiece pos p) <- enumVec $ placedPieces st
	    , or [wsel && isWrench p, not wsel && isHook p] ]

    drawMainState (EditState (st:_) _ _ _ selPiece selPos _) = do
	drawState (maybeToList selPiece) True [] st
	drawCursorAt $ if isNothing selPiece then Just selPos else Nothing

    drawMainState (MetaState saddr cOnly auth names _ uinfo path lock) = do
	(h,w) <- liftIO Curses.scrSize
	let name = liftM authUser auth
	erase
	drawStrGrey (CVec 0 0) $ "Server: " ++ saddrStr saddr 
	drawStrGrey (CVec 1 0) $ "Lock: " ++ fromMaybe "" path
	drawStrGrey (CVec 2 (w`div`2-1)) $ fromMaybe "" $ listToMaybe names
	case uinfo of
	    Nothing -> return ()
	    Just (uiTVar, lockTVars) -> do
		FetchedRecord fresh err muirc <- liftIO $ atomically $ readTVar uiTVar
		when (not fresh) $ drawAtCVec (Glyph '*' red bold) $ CVec 2 (w`div`2+4)
		maybe (return ()) sayError err
	refresh

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert (AlertCollision pos) = drawAt cGlyph pos
	    drawAlert _ = return ()
	    cGlyph = Glyph '!' 0 a0

    drawMessage prompt = say prompt

    initUI = 
	do liftIO $ CursesH.start 
	   cpairs <- liftIO $ colorsToPairs [ (f, CursesH.black) | f <-
		[ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue, CursesH.magenta] ] 
	   modify $ \s -> s {dispCPairs = cpairs}
	   readBindings
	   return True

    endUI = liftIO CursesH.end
    unblockInput = return $ Curses.ungetCh 0

    getInput mode = do
	key <- liftIO $ CursesH.getKey Curses.refresh
	case key of
	    Curses.KeyChar ch ->
		if mode == IMTextInput
		then return [CmdInputChar ch]
		else do
		    uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
		    return $ maybeToList $ lookup ch $ uibdgs ++ bindings mode
	    _ -> return []
