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
import Data.Array
import Data.Maybe
import Data.List
import Control.Monad.Trans.Maybe
import Control.Concurrent (threadDelay)
import Control.Monad.State
import Data.Function (on)

import Hex
import GameState
import Physics
import Frame
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
      | otherwise = '?'
    a = if principal then bold else a0
    in (c,a)
tileChar HookTile = ('@',bold)
tileChar (WrenchTile mom) = ('*',if mom /= zero then bold else a0)
tileChar BallTile = ('O',a0)
tileChar (SpringTile Relaxed _) = ('S',a0)
tileChar (SpringTile Compressed _) = ('$',bold)
tileChar (SpringTile Stretched _) = ('s',bold)
tileChar _ = ('?',bold)

ownedTileGlyph :: PieceColouring -> [PieceIdx] -> OwnedTile -> Glyph
ownedTileGlyph colouring reversed (owner,t) =
    let (ch,attr) = tileChar t
	pair = case Map.lookup owner colouring of
		    Nothing -> 0
		    Just n -> n+1
	rev = owner `elem` reversed
    in Glyph ch pair (Curses.setReverse attr rev)

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
mvAddStr (CVec y x) = Curses.mvWAddStr Curses.stdScr y x
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

getBindings mode = do
    uibdgs <- Map.findWithDefault [] mode `liftM` gets uiKeyBindings
    return $ uibdgs ++ bindings mode

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
    erase
    colouring <- drawStateWithGeom reversed colourFixed lastCol st =<< getGeom
    modify $ \ds -> ds { dispLastCol = colouring }

-- TODO: monochrome mode
drawStateWithGeom reversed colourFixed lastCol st geom = do
    let colouring = boardColouring st (colouredPieces colourFixed st) lastCol
    sequence_ [ drawAtWithGeom glyph pos geom | 
	(pos,glyph) <- Map.toList $ fmap (ownedTileGlyph colouring reversed) $ stateBoard st
	]
    return colouring

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
	    return $ replicate nColors $ Curses.Pair 0
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

drawName :: Bool -> CVec -> Codename -> StateT MainState UIM ()
drawName showScore pos name = do
    ourName <- (authUser <$>) <$> gets curAuth
    relScore <- getRelScore name
    let (attr,col) = case relScore of
	    Just 0 -> (a0,yellow)
	    Just 1 -> (bold,cyan) 
	    Just 2 -> (a0,green) 
	    Just 3 -> (bold,green) 
	    Just (-1) -> (bold,magenta)
	    Just (-2) -> (a0,red)
	    Just (-3) -> (bold,red)
	    _ -> if ourName == Just name then (bold,white) else (a0,white)
    lift $ drawStrCentred attr col pos
	(name ++ if showScore then " " ++ maybe "" show relScore else "")

drawActiveLock :: CVec -> ActiveLock -> StateT MainState UIM ()
drawActiveLock pos (ActiveLock name i) = do
    drawName False (pos <+> CVec 0 (-1)) name
    lift $ drawStr bold white (pos <+> CVec 0 1) [':',lockIndexChar i]

data Gravity = GravUp | GravLeft | GravRight | GravDown | GravCentre
    deriving (Eq, Ord, Show, Enum)
fillBox :: CVec -> CVec -> Int -> Gravity -> [CVec -> StateT MainState UIM ()] -> StateT MainState UIM Int
fillBox (CVec t l) (CVec b r) width grav draws = do
    let half = width`div`2
	starty = (if grav == GravDown then b else t)
	cv = (b+t)`div`2
	ch = (l+r)`div`2
	gravCentre = case grav of
	    GravDown   -> CVec b ch
	    GravUp     -> CVec t ch
	    GravLeft   -> CVec cv l
	    GravRight  -> CVec cv r
	    GravCentre -> CVec cv ch
	locs = sortBy (compare `on` dist) $ concat
	    [ map (CVec j) [ l+margin + (width+1)*i | i <- [0..(r-l-(2*margin))`div`(width+1)] ]
	    | j <- [t..b]
	    , let margin = if (j-starty)`mod`2 == 0 then half else width ]
	dist v = sqlen $ v <-> gravCentre
	sqlen (CVec y x) = (y*(width+1))^2+x^2
    selDraws <- do
	    offset <- gets listOffset
	    let na = length locs
		nd = length draws
	    return $ drop (max 0 $ min (nd - na) (na*offset)) $ draws
    let zipped = zip locs selDraws
    sequence_ $ map (uncurry ($)) $ zip selDraws locs
    return $ (if grav==GravDown then minimum else maximum) [ y | (CVec y x,_) <- zipped ]

drawLockInfo al@(ActiveLock name i) lockinfo mtvar = do
    (h,w) <- liftIO Curses.scrSize
    let [left,vcentre,right] = [ (k+2*i)*w`div`6 + (1-k) | k <- [0,1,2] ]
    let [top,bottom] = [6, h-1]
    let hcentre = (top+bottom)`div`2 - 1
    ourName <- (authUser <$>) <$> gets curAuth

    (lockTop, lockBottom) <- (fromJust<$>)$ runMaybeT $ msum
	[ do
	    tvar <- MaybeT $ return mtvar
	    lock <- MaybeT $ liftIO.atomically.readTVar $ tvar
	    let size = frameSize $ fst lock
	    guard $ bottom - top >= 5 + 2*size+1 + 1 + 5 && right-left >= 4*size+1
	    lift.lift $ drawStateWithGeom [] False Map.empty (snd lock) (CVec hcentre vcentre,origin)
	    return (hcentre - size - 1, hcentre + size + 1)
	, lift $ do
	    drawActiveLock (CVec hcentre vcentre) al
	    return (hcentre - 1, hcentre + 1)
	]

    startOn <-
	if (public lockinfo) 
	then lift $ drawStrCentred bold white (CVec (lockTop-1) vcentre) "Everyone!"
	    >> return (lockTop-1)
	else if null $ accessedBy lockinfo
	    then lift $ drawStrCentred a0 white (CVec (lockTop-1) vcentre) "No-one"
		>> return (lockTop-1)
	    else
		fillBox (CVec (top+1) (left+1)) (CVec (lockTop-1) (right-1)) 3 GravDown $
		    [ \pos -> drawName False pos name | name <- accessedBy lockinfo ]
    lift $ drawStrCentred a0 white (CVec (startOn-1) vcentre) "Accessed by:"
    
    undecls <- gets undeclareds
    if isJust $ guard . (|| public lockinfo) . (`elem` accessedBy lockinfo) =<< ourName
    then lift $ drawStrCentred a0 green (CVec (lockBottom+1) vcentre) "Accessed!"
    else if any (\(Undeclared _ ls _) -> ls == lockSpec lockinfo) undecls
    then lift $ drawStrCentred a0 yellow (CVec (lockBottom+1) vcentre) "Undeclared solution!"
    else do
	read <- take 3 <$> getNotesReadOn lockinfo
	unless (null read || ourName == Just name) $ do
	    let rntext = if right-left > 30 then "Read notes by:" else "Notes:"
		s = vcentre - (length rntext+(3+1)*3)`div`2
	    lift $ drawStr a0 white (CVec (lockBottom+1) s) rntext
	    void $ fillBox (CVec (lockBottom+1) (s+length rntext+1)) (CVec (lockBottom+1) right) 3 GravLeft
		[ \pos -> drawName False pos name | name <- map noteAuthor read ]

    lift $ drawStrCentred a0 white (CVec (lockBottom+2) vcentre) "Notes held:"
    if null $ notesSecured lockinfo
	then lift $
	    drawStrCentred a0 white (CVec (lockBottom+3) vcentre) "None"
	else
	    void $ fillBox (CVec (lockBottom+3) (left+1)) (CVec bottom (right-1)) 5 GravUp
		[ (`drawActiveLock` al) | al <- map noteOn $ notesSecured lockinfo ]


instance UIMonad (StateT UIState IO) where
    runUI m = evalStateT m nullUIState

    drawMainState = get >>= \s -> case s of
	PlayState st _ alerts wsel _ _ _ title _ -> lift $ do
	    drawState [] False alerts st
	    drawMsgLine
	    drawTitle title
	    drawCursorAt $ listToMaybe [ pos |
		(_, PlacedPiece pos p) <- enumVec $ placedPieces st
		, or [wsel && isWrench p, not wsel && isHook p] ]
	    refresh
	ReplayState st _ _ title -> lift $ do
	    drawState [] False [] st
	    drawMsgLine
	    drawTitle title
	    drawCursorAt Nothing
	    refresh
	EditState (st:_) _ _ _ selPiece selPos _ _ -> lift $ do
	    drawState (maybeToList selPiece) True [] st
	    drawMsgLine
	    drawCursorAt $ if isNothing selPiece then Just selPos else Nothing
	    refresh
	MetaState saddr undecls cOnly auth names _ rnamestvar _ lockTVs path lock _ -> do
	    let ourName = liftM authUser auth
	    let selName = listToMaybe names
	    (h,w) <- liftIO Curses.scrSize
	    lift $ do
		erase
		drawCursorAt Nothing
		drawStrGrey (CVec 0 0) $ "Server: " ++ saddrStr saddr 
		drawStrGrey (CVec 1 0) $ "Lock: " ++ path
	    maybe (return ()) (drawName True (CVec 2 (w`div`2))) selName
	    void.runMaybeT $ MaybeT (return selName) >>= lift . getUInfoFetched 300 >>=
		\(FetchedRecord fresh err muirc) -> lift $ do
		    lift $ do
			unless fresh $ drawAtCVec (Glyph '*' red bold) $ CVec 2 (w`div`2+7)
			maybe (return ()) sayError err
			when (fresh && isNothing ourName) $
			    drawStrGrey (CVec 2 (w`div`2+1+9)) (if isNothing muirc then "(R)" else "(A)")
		    case muirc of
			Nothing -> return ()
			Just (RCUserInfo (_,uinfo)) ->
			    sequence_ [ drawLockInfo (ActiveLock (codename uinfo) i) lockinfo ((!i)<$>lockTVs) |
				(i,Just lockinfo) <- assocs $ userLocks uinfo ]
	    when (isJust ourName && ourName == selName) $ do
		unless (null undecls) $ do
		    let str = "Undeclared solutions:"
		    let s = max 1 $ (w-(length str + 6*length undecls))`div`2
		    let y = 4
		    lift $ drawStr bold white (CVec y s) str
		    void $ fillBox (CVec y (s+length str+1)) (CVec y (w-1)) 5 GravLeft
			[ (`drawActiveLock` al) | Undeclared _ _ al <- undecls ]
		rnames <- liftIO $ atomically $ readTVar rnamestvar
		unless (null rnames) $
		    void $ fillBox (CVec 2 5) (CVec 5 (w`div`3)) 3 GravCentre
			[ \pos -> drawName False pos name | name <- rnames ]
	    when (ourName /= selName) $ void $ runMaybeT $ do
		sel <- MaybeT $ return selName
		us <- MaybeT $ return ourName
		ourUInfo <- mgetUInfo us
		let accessed = [ ActiveLock us i 
			| i<-[0..2]
			, Just lock <- [ userLocks ourUInfo ! i ]
			, public lock || selName `elem` map Just (accessedBy lock) ]
		guard $ not $ null accessed
		let str = "has accessed:"
		let s = (w-(4 + length str + 6*(length accessed)))`div`2
		let y = 4
		lift $ do
		    drawName False (CVec y (s+1)) sel
		    lift $ drawStrGrey (CVec y $ s+4) str
		    void $ fillBox (CVec y (s+4+length str+1)) (CVec y (w-1)) 5 GravLeft $
			[ (`drawActiveLock` al) | al <- accessed]
	    lift drawMsgLine
	    lift refresh

    drawAlerts alerts =
	do mapM_ drawAlert alerts
	   unless (null alerts) 
	    $ do refresh
		 liftIO $ threadDelay $ 5*10^4
	where
	    drawAlert (AlertCollision pos) = drawAt cGlyph pos
	    drawAlert _ = return ()
	    cGlyph = Glyph '!' 0 a0

    drawMessage = say
    drawError = sayError

    showHelp mode = do
	bdgs <- nub <$> getBindings mode
	erase
	(h,w) <- liftIO Curses.scrSize
	let bdgWidth = 35
	drawStrCentred a0 white (CVec 0 (w`div`2)) "Bindings:"
	sequence_ [ drawStr a0 white (CVec (y+2) (x*bdgWidth) ) $
		intercalate " / " (map showKey chs) ++ " : " ++ describeCommand cmd |
	    ((chs,cmd),(x,y)) <- zip [(chs,cmd)
		    | group <- groupBy ((==) `on` snd) $ sortBy (compare `on` snd) bdgs
		    , let cmd = snd $ head group
		    , let chs = map fst group]
		(map (`divMod` (h-3)) [0..])
	    , (x+1)*bdgWidth < w]
	refresh
	void $ getInput mode

    initUI = 
	do liftIO CursesH.start 
	   cpairs <- liftIO $ colorsToPairs [ (f, CursesH.black) | f <-
		[ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue, CursesH.magenta, CursesH.cyan] ] 
	   modify $ \s -> s {dispCPairs = cpairs}
	   readBindings
	   return True

    endUI = liftIO CursesH.end
    unblockInput = return $ Curses.ungetCh 0

    getInput mode = do
	-- FIXME: SIGWINCH isn't being handled properly
	key <- liftIO $ CursesH.getKey Curses.refresh
	let mch = case key of
		Curses.KeyChar ch -> Just ch
		Curses.KeyBackspace -> Just '\b'
		Curses.KeyLeft -> Just '4'
		Curses.KeyRight -> Just '6'
		Curses.KeyDown -> Just '2'
		Curses.KeyUp -> Just '8'
		Curses.KeyHome -> Just '7'
		Curses.KeyNPage -> Just '3'
		Curses.KeyPPage -> Just '9'
		Curses.KeyEnd -> Just '1'
		_ -> Nothing
	flip (maybe $ return []) mch $ \ch ->
	    if mode == IMTextInput
	    then return [CmdInputChar ch]
	    else (maybeToList . lookup ch) <$> getBindings mode

white = 0 :: ColPair
red = 1
green = 2
yellow = 3
blue = 4
magenta = 5
cyan = 6
