{-# LANGUAGE FlexibleInstances #-}
module CursesUI ( UIM ) where

import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Concurrent.STM
import Control.Applicative
import Data.Char (ord)
import Data.Monoid
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Array as Array
import Data.Maybe
import Control.Monad.Trans.Maybe
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
import Database
import Metagame
import Protocol
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
    , dispLastCol::PieceColouring, uiKeyBindings :: Map InputMode KeyBindings
    , message::Maybe (Curses.Attr, ColPair, String)}
type UIM = StateT UIState IO
nullUIState = UIState [] (PHS zero) Map.empty Map.empty Nothing

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
drawStrCentred attr col v str = do
    drawStr attr col (v <+> CVec 0 (-length str `div` 2)) str

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

drawMsgLine = void.runMaybeT $ do
    (attr,col,str) <- MaybeT $ gets message
    lift $ do
	(h,w) <- liftIO Curses.scrSize
	liftIO $ clearLine $ h-1
	drawStr attr col (CVec (h-1) 0) str
	refresh
setMsgLine attr col str = do
    modify $ \s -> s { message = Just (attr,col,str) }
    drawMsgLine
    
say = setMsgLine bold white
sayError = setMsgLine bold red

drawName :: CVec -> Codename -> StateT MainState UIM ()
drawName pos name = do
    ourName <- (authUser <$>) <$> gets curAuth
    relScore <- getRelScore name
    let (attr,col) = if ourName == Just name
	    then (bold,white)
	    else case relScore of
		Nothing -> (a0,white)
		Just 0 -> (a0,yellow)
		Just 1 -> (a0,green) 
		Just 2 -> (bold,green) 
		Just 3 -> (bold,cyan) 
		Just (-1) -> (a0,red)
		Just (-2) -> (bold,red)
		Just (-3) -> (bold,magenta)
    lift $ drawStr attr col (pos <+> CVec 0 (-1)) name

drawActiveLock :: CVec -> ActiveLock -> StateT MainState UIM ()
drawActiveLock pos (ActiveLock name i) = do
    drawName (pos <+> CVec 0 (-1)) name
    lift $ drawStrGrey (pos <+> CVec 0 1) ":"
    lift $ drawStr bold white (pos <+> CVec 0 2) [lockLetter i]

fillBox :: CVec -> CVec -> Int -> [CVec -> StateT MainState UIM ()] -> StateT MainState UIM ()
fillBox (CVec t l) (CVec b r) width draws =
    let half = width`div`2
	locs = concat
	    [ map (CVec j) [ l+half + (width+1)*i | i <- [0..(r-l-(2*half))`div`(width+1)] ]
	    | j <- [t..b] ]
    in sequence_ [draw pos | (pos,draw) <- zip locs draws]

lockLetter i = toEnum . (+i) . fromEnum $ 'A'

drawLockInfo al@(ActiveLock name i) lockinfo = do
    (h,w) <- liftIO Curses.scrSize
    let [left,vcentre,right] = [ (k+2*i)*w`div`6 + (1-k) | k <- [0,1,2] ]
    let [top,bottom] = [4, h-7]
    let hcentre = (top+bottom)`div`2 - 1
    ourName <- (authUser <$>) <$> gets curAuth

    drawActiveLock (CVec hcentre vcentre) al

    lift $ drawStrCentred a0 white (CVec top vcentre) "Accessed by:"
    if (public lockinfo) 
	then lift $ drawStr bold white (CVec (top+1) (left+2)) "Everyone!"
	else if null $ accessedBy lockinfo
	    then lift $
		drawStr a0 white (CVec (top+1) (left+2)) "No-one"
	    else
		fillBox (CVec (top+1) (left+1)) (CVec (hcentre-1) (right-1)) 3 $
		    [ \pos -> drawName pos name | name <- accessedBy lockinfo ]
    
    if isJust $ guard . (`elem` accessedBy lockinfo) =<< ourName
	then lift $ drawStrCentred a0 green (CVec (hcentre+2) vcentre) "Accessed!"
	else do
	    read <- getNotesReadOn al
	    when (not $ null read) $ do
		lift $ drawStr a0 white (CVec (hcentre+2) left) "Read notes: "
		fillBox (CVec (hcentre+2) (left+12)) (CVec (hcentre+2) right) 3 $
		    [ \pos -> drawName pos name | name <- map noteAuthor read ]

    lift $ drawStrCentred a0 white (CVec (hcentre+3) vcentre) "Notes held:"
    if null $ notesSecured lockinfo
	then lift $
	    drawStr a0 white (CVec (hcentre+4) (left+2)) "None"
	else
	    fillBox (CVec (hcentre+4) (left+1)) (CVec (bottom) (right-1)) 5 $
		[ \pos -> drawActiveLock pos al | al <- map noteOn $ notesSecured lockinfo ]


instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState

    drawMainState = get >>= \s -> case s of
	PlayState st _ alerts wsel solved _ _ -> lift $ do
	    drawState [] False alerts st
	    when solved $ say "Unlocked!"
	    drawMsgLine
	    drawCursorAt $ listToMaybe [ pos |
		(_, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]
	EditState (st:_) _ _ _ selPiece selPos _ -> lift $ do
	    drawState (maybeToList selPiece) True [] st
	    drawMsgLine
	    drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	MetaState saddr undecls cOnly auth names _ _ path lock -> do
	    let ourName = liftM authUser auth
	    let selName = listToMaybe names
	    (h,w) <- liftIO Curses.scrSize
	    lift $ do
		erase
		drawCursorAt Nothing
		drawStrGrey (CVec 0 0) $ "Server: " ++ saddrStr saddr 
		drawStrGrey (CVec 1 0) $ "Lock: " ++ path
	    maybe (return ()) (drawName (CVec 2 (w`div`2))) selName
	    runMaybeT $ MaybeT (return selName) >>= lift . getUInfoFetched 30 >>=
		\(FetchedRecord fresh err muirc) -> lift $ do
		    lift $ do
			when (not fresh) $ drawAtCVec (Glyph '*' red bold) $ CVec 2 (w`div`2+4)
			maybe (return ()) sayError err
			when (fresh && isNothing ourName) $
			    drawStrGrey (CVec 2 (w`div`2+1+6)) (if isNothing muirc then "(R)" else "(A)")
		    case muirc of
			Nothing -> return ()
			Just (RCUserInfo uinfo) -> do
			    sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) lockinfo |
				(i,Just lockinfo) <- Array.assocs $ userLocks uinfo ]
	    when (isJust ourName && ourName == selName) $ do
		when (not $ null undecls) $ do
		    let str = "Undeclared solutions: "
		    let line = h-5
		    lift $ drawStr bold white (CVec line 1) str
		    fillBox (CVec line (1+length str)) (CVec line (w-1)) 5 $
			[ \pos -> drawActiveLock pos al | Undeclared _ _ al <- undecls ]
	    lift $ drawMsgLine
	    lift $ refresh

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   when (not $ null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert (AlertCollision pos) = drawAt cGlyph pos
	    drawAlert _ = return ()
	    cGlyph = Glyph '!' 0 a0

    drawMessage str = say str
    drawError str = sayError str

    initUI = 
	do liftIO $ CursesH.start 
	   cpairs <- liftIO $ colorsToPairs [ (f, CursesH.black) | f <-
		[ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue, CursesH.magenta, CursesH.cyan] ] 
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

white = 0 :: ColPair
red = 1
green = 2
yellow = 3
blue = 4
magenta = 5
cyan = 6
