module Main where

import Hex
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Data.List (intersect, union, (\\), elemIndices, nub)
import Data.Monoid
import Data.Maybe
import Control.Monad.State
import Data.Vector (Vector, (!), (!?), (//))
import qualified Debug.Trace as Trace
import Text.Show.Pretty (ppShow)
import Data.Char (ord)

import UI.HSCurses.Logging
import UI.HSCurses.Widgets
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH

enumVec :: Vector a -> [(Int,a)]
enumVec = Vector.toList . Vector.indexed

prettyTrace :: Show a => a -> b -> b
prettyTrace = Trace.trace . ppShow
prettyTraceVal :: Show a => a -> a
prettyTraceVal x = prettyTrace x x

type HexPos = PHS HexVec

data GameState = GameState { placedPieces :: Vector PlacedPiece,
	connections :: [Connection] }
    deriving (Eq, Ord, Show)

data PlacedPiece = PlacedPiece { placedPos :: HexPos, placedPiece :: Piece }
    deriving (Eq, Ord, Show)

getpp :: GameState -> PieceIdx -> PlacedPiece
getpp st idx = fromJust $ (placedPieces st) !? idx

setPiece :: GameState -> PieceIdx -> Piece -> GameState
setPiece st@(GameState pps _) idx p = 
    st { placedPieces = pps //
	[(idx, PlacedPiece (placedPos $ getpp st idx) p)] }

data Piece = Block { blockPattern :: [HexVec] }
	    | Pivot { pivotArms :: [HexDir] }
	    | Hook { hookArm :: HexDir }
	    | Wrench { wrenchMomentum :: HexDir }
	    | Ball { ballMomentum :: HexDir }
    deriving (Eq, Ord, Show)

type PieceIdx = Int

type Locus = (PieceIdx, HexVec)
locusPos :: GameState -> Locus -> HexPos
locusPos s (idx,v) = v <+> (placedPos $ getpp s idx)

data Connection = Connection { connectionRoot :: Locus,
		    connectionEnd :: Locus, link :: Link }
    deriving (Eq, Ord, Show)

connectionLength :: GameState -> Connection -> Int
connectionLength st (Connection root end _) =
    let rootPos = locusPos st root
	endPos = locusPos st end
    in hexLen (endPos <-> rootPos) - 1

data Link = Free { freePos :: HexVec }
	  | Spring { springDir :: HexDir, springNatLength :: Int }
    deriving (Eq, Ord, Show)

springsAtIdx st idx =
    [ c | c@(Connection _ (eidx, _) (Spring _ _)) <- connections st
	  , eidx==idx ]

data SpringExtension = Relaxed | Compressed | Stretched
    deriving (Eq, Ord, Show)

data Tile = BlockTile | PivotTile | ArmTile HexDir Bool | HookTile | WrenchTile Bool
	  | BallTile Bool | SpringTile SpringExtension
    deriving (Eq, Ord, Show)

type OwnedTile = (PieceIdx, Tile)
type GameBoard = Map HexPos OwnedTile

stateBoard :: GameState -> GameBoard
stateBoard st@(GameState plPieces conns) =
    (Map.unions $ map plPieceBoard $ enumVec plPieces) `Map.union`
	  (Map.unions $ map (connectionBoard st) conns)

plPieceBoard :: (PieceIdx,PlacedPiece) -> GameBoard
plPieceBoard (idx,pp) = Map.map (\x -> (idx,x)) $ plPieceMap pp

plPieceMap :: PlacedPiece -> Map HexPos Tile
plPieceMap (PlacedPiece pos (Block pattern)) =
    Map.fromList [ (rel <+> pos, BlockTile) | rel <- pattern ]
plPieceMap (PlacedPiece pos (Pivot arms)) =
    Map.fromList $ (pos, PivotTile) : 
	[ (rel <+> pos, ArmTile rel main) |
	    (rel,main) <- zip arms (True:repeat False)]
plPieceMap (PlacedPiece pos (Hook arm)) =
    Map.fromList $ (pos, HookTile) : [ (arm <+> pos, ArmTile arm True) ]
plPieceMap (PlacedPiece pos (Wrench mom)) = Map.singleton pos $ WrenchTile (mom /= zero)
plPieceMap (PlacedPiece pos (Ball mom)) = Map.singleton pos $ BallTile (mom /= zero)

plPieceFootprint :: PlacedPiece -> [HexPos]
plPieceFootprint = Map.keys . plPieceMap

-- TODO: use bounding boxes to optimise collision detection?
collideEntities :: PlacedPiece -> PlacedPiece -> [HexPos]
collideEntities pp pp' =
    intersect (plPieceFootprint pp) (plPieceFootprint pp')
entitiesIntersect :: PlacedPiece -> PlacedPiece -> Bool
entitiesIntersect pp pp' = not $ null $ collideEntities pp pp'

connectionBoard :: GameState -> Connection -> GameBoard
connectionBoard st (Connection root end@(eidx,_) (Spring dir natLen)) =
    let rootPos = locusPos st root
	endPos = locusPos st end
	curLen = hexLen (endPos <-> rootPos) - 1
    in Map.fromList $
	[ ((d <*> dir) <+> rootPos, (eidx, SpringTile extension))
	    | d <- [1..curLen],
	    let extension | d <= natLen - curLen = Compressed
			  | curLen-d < 2*(curLen - natLen) = Stretched
			  | otherwise = Relaxed ]
connectionBoard _ _ = Map.empty

connectionFootPrint :: GameState -> Connection -> [HexPos]
connectionFootPrint s c = Map.keys $ connectionBoard s c

pieceIntersectsConnection :: GameState -> PlacedPiece -> Connection -> Bool
pieceIntersectsConnection s pp c =
    not $ null $ intersect (plPieceFootprint pp) (connectionFootPrint s c)


type PieceColouring = Map PieceIdx Int
type Graph a = (Set a, Set (Set a))

-- TODO: try previous colouring first, and pref aim to make minimal changes
-- to it if it's no longer valid
boardColouring :: GameBoard -> [PieceIdx] -> PieceColouring
boardColouring board coloured = fourColour adjGraph
    where adjGraph = foldl addPoint (Set.empty,Set.empty) $ Map.toList board
	  addPoint :: (Graph Int) -> (HexPos, OwnedTile) -> (Graph Int)
	  addPoint g@(nodes, edges) (pos, (owner,_)) 
	    | not $ owner `elem` coloured = g
	    | otherwise = (Set.insert owner nodes, edges `Set.union`
		Set.fromList [ Set.fromList [owner,owner'] |
			pos' <- map (\d -> d<+>pos) hexDirs
			, Just (owner',_) <- [Map.lookup pos' board]
			, owner' /= owner ]
		)

fourColour :: Ord a => Graph a -> Map a Int
fourColour (nodes,edges) =
    -- Just bruteforce it. TODO: properly.
    head $ filter isColouring colourings
    where 
	isColouring mapping = and [
	    Map.lookup s mapping /= Map.lookup e mapping |
		edge <- Set.toList edges
		, [s,e] <- [Set.toList edge] ]
	colourings = colourings' $ Set.toList nodes
	colourings' [] = [ Map.empty ]
	colourings' (n:ns) = [ Map.insert n c m |
	    m <- colourings' ns
	    , c <- [0..3] ]
	

-- TorqueDir: Int of absolute value <= 1
type TorqueDir = Int

-- Crappy terminology: 'force' encompasses both usual directional forces and
-- torques; we use 'push' for the former.
data Force = Push PieceIdx HexDir | Torque PieceIdx TorqueDir
    deriving (Eq, Ord, Show)
forceIdx force = case force of (Push idx _) -> idx
			       (Torque idx _) -> idx
getForcedpp :: GameState -> Force -> PlacedPiece
getForcedpp s f = getpp s (forceIdx f)

type Forces = Vector.Vector Force

data PlayerMove = PlayerMove { hookPush :: HexDir, hookTorque :: TorqueDir, wrenchPush :: HexDir }
    deriving (Eq, Ord, Show)
nullMove = PlayerMove zero 0 zero

toolForces :: GameState -> PlayerMove -> Forces
toolForces st (PlayerMove hp ht wp) = Vector.fromList $
    [ Push widx dir |
	(widx, PlacedPiece _ (Wrench mom)) <- epps
	, let dir = if mom == zero then wp else mom ] ++
    ( if hp == zero then [] else
	[ Push hidx hp | (hidx, PlacedPiece _ (Hook _)) <- epps ] ) ++
    ( if ht == 0 then [] else
	[ Torque hidx ht | (hidx, PlacedPiece _ (Hook _)) <- epps ] )
    where epps = enumVec $ placedPieces st

envForces :: GameState -> ([PieceIdx], Forces)
envForces st@(GameState plPieces conns) =
    (catMaybes stoppedBallMbs, Vector.fromList $ springForces ++ ballForces)
    where
	springForces = [ Push endIdx dir | c@(Connection _ (endIdx, _) (Spring outDir natLen)) <- conns
			    , let curLen = connectionLength st c
			    , natLen /= curLen
			    , let dir = if natLen > curLen then outDir else neg outDir ]
	(stoppedBallMbs, ballForces) = unzip [ (stoppedBall, force) |
	    (ballIdx, PlacedPiece _ (Ball momentum)) <- enumVec plPieces
	    , momentum /= zero
	    , let ballForce = Push ballIdx momentum
	    , let propagated = propagateForce st ballForce
	    , let force = fromMaybe ballForce $ listToMaybe propagated
	    , let stoppedBall = if not $ null propagated then Just ballIdx else Nothing
	    ]

physicsTick :: GameState -> PlayerMove -> GameState
physicsTick st move =
    let st' = resolveForces st (toolForces st move) True
	(stoppedBalls, forces) = envForces st'
	stopBall st'' idx = setPiece st'' idx (Ball zero)
	stopBalls st'' = foldl stopBall st'' stoppedBalls
	in stopBalls $ resolveForces st' forces False

type Source = Int
type SourcedForce = (Source,Force)
resolveForces :: GameState -> Forces -> Bool -> GameState
resolveForces st forces isPlayer =
    let
	resolveForce :: SourcedForce -> State ([SourcedForce], [Source]) ()
	resolveForce sforce = do
	    (sforces,blockedsources) <- get
	    let blocks = checkBlocks st sforces sforce isPlayer
	    let propagated = [ (fst sforce, f) | f <- propagateForce st (snd sforce) ] \\ sforces
	    put (propagated ++ (sforce:sforces), blocks ++ blockedsources)
	    sequence_ $ map resolveForce propagated
	sforces = enumVec forces
	(sforces', blocked) = (sequence_ (map resolveForce sforces)) `execState` ([],[])
	-- stop wrenches which are blocked
	stopWrench st' idx = setPiece st' idx (Wrench zero)
	stopWrenches st' = foldl stopWrench st' [
	    forceIdx $ forces!s | s <- blocked
	    , (Wrench _) <- [placedPiece $ getForcedpp st' $ forces!s] ]
	unblocked = nub [ force | (s,force) <- sforces', not $ s `elem` blocked ]
	applyForces st' = foldl applyForce st' unblocked
    in --prettyTrace (st, sforces, sforces', blocked, unblocked) $
	stopWrenches $ applyForces st
    
applyForce :: GameState -> Force -> GameState
applyForce s f =
    let idx = forceIdx f
	pp' = applyForceTo (getpp s idx) f
	pp'' = case (placedPiece pp',f) of
		   -- update momentum
		   ( (Ball _), (Push _ dir) ) -> pp' {placedPiece = (Ball dir)}
		   ( (Wrench _), (Push _ dir) ) -> pp' {placedPiece = (Wrench dir)}
		   _ -> pp'
	in 
	s { placedPieces = (placedPieces s) // [(idx, pp'')] }
applyForceTo :: PlacedPiece -> Force -> PlacedPiece
applyForceTo (PlacedPiece pos piece) (Push _ dir) =
    PlacedPiece (dir <+> pos) piece
applyForceTo (PlacedPiece pos (Pivot arms)) (Torque _ dir) =
    PlacedPiece pos (Pivot $ map (rotate dir) arms)
applyForceTo (PlacedPiece pos (Hook arm)) (Torque _ dir) =
    PlacedPiece pos (Hook $ rotate dir arm)

checkBlocks :: GameState -> [SourcedForce] -> SourcedForce -> Bool -> [Source]
checkBlocks st@(GameState plPieces conns) sforces sforce@(source, force) isPlayer =
    let idx = forceIdx force
	pp@(PlacedPiece pos piece) = (getpp st idx)
	pp' = applyForceTo pp force
        springs = springsAtIdx st idx
	blockingSprings = [ c |
		    c@(Connection _ (eidx, _) (Spring sdir natLen)) <- springs
		    , idx == eidx
		    , let curLen = connectionLength st c
		    , (Push _ pdir) <- [force]
		    , not (pdir `elem` [sdir, neg sdir]) || (
			not isPlayer &&
			    ( (curLen < natLen && pdir /= sdir) ||
				(curLen > natLen && pdir /= neg sdir) ) ) ]
	linkCollisions = [ c | c <- conns
	    , (fst . connectionEnd) c /= idx
	    , (fst . connectionRoot) c /= idx
	    , pieceIntersectsConnection st pp' c ]
	fixed = case piece of
		     (Pivot _) -> case force of {(Push _ _) -> True; _ -> False}
		     (Wrench _) -> not isPlayer
		     (Hook _) -> not isPlayer
		     (Block _) -> null springs
		     _ -> False
        mismatches = [ s | (s,force') <- sforces, idx == forceIdx force', force /= force' ]
	pieceCollisions = [ s |
	    (s,force') <- sforces
	    , let idx' = forceIdx force'
	    , idx /= idx'
	    , entitiesIntersect pp'
		(applyForceTo (getpp st idx') force')]
	blocks = mismatches ++ pieceCollisions
	blocked = or [not $ null blocks
	    , not $ null blockingSprings
	    , not $ null linkCollisions
	    , fixed]
    in if blocked then source:blocks else blocks

immediatelyBlocked :: GameState -> Force -> Bool
immediatelyBlocked = immediatelyBlocked' False
immediatelyBlocked' recursed st@(GameState pps conns) f@(Push idx dir) =
    let pp@(PlacedPiece pos piece) = getpp st idx
	springs = springsAtIdx st idx
        blockingSprings = [ c |
	    c@(Connection _ _ (Spring sdir _)) <- springs
	    , not $ dir `elem` [sdir, neg sdir] ]
	isPivot = case piece of (Pivot _) -> True; _ -> False
	pieceCollisions = [ idx' |
	    (idx', pp') <- enumVec $ pps
	    , idx /= idx'
	    , entitiesIntersect (applyForceTo pp' f) pp' ]
	fixed = case piece of
		     (Block _) -> or [null springs, not $ null blockingSprings]
		     (Pivot _) -> True
		     (Wrench _) -> True
		     (Hook _) -> True
		     _ -> False
    in or [ fixed
	    , and [not isPivot, not recursed, not $ null pieceCollisions
		  , immediatelyBlocked' True st (Push (head pieceCollisions) dir) ] ]
immediatelyBlocked' _ _ (Torque _ _) = False


-- FIXME: torques on hooks getting generated, blocking pushes with which
-- they're really consistent... maybe should be treating centre and arm of
-- the hook as separate pieces?
propagateForce :: GameState -> Force -> [Force]
propagateForce st@(GameState plPieces conns) force = 
    bumps ++ springTransfers
    where
	bumps = [ transferredForce idx' cpos dir |
	    let pp = applyForceTo (getForcedpp st force) force
	    , (idx',pp') <- enumVec plPieces
	    , idx' /= forceIdx force
	    , cpos <- collideEntities pp pp'
	    , let dir = case force of
		    Push _ dir -> dir
		    Torque _ dir -> if immediatelyBlocked st (transferredForce idx' cpos push)
					 then claw else push
			where push = arm <-> rotate (-dir) arm
			      claw = rotate dir arm <-> arm
			      arm = cpos <-> placedPos pp ]
	transferredForce :: Source -> HexPos -> HexDir -> Force
	transferredForce idx cpos dir =
	    let pp@(PlacedPiece pos piece) = getpp st idx
		rpos = cpos <-> placedPos pp
		armPush = case
			(dir `hexDot` ((rotate 1 rpos) <-> rpos)) `compare`
			(dir `hexDot` ((rotate (-1) rpos) <-> rpos)) of
		    GT -> Torque idx 1
		    LT -> Torque idx $ -1
		    EQ -> Push idx dir
	    in
		case piece of
		    Pivot _ -> armPush
		    Hook _ -> armPush
		    _ -> (Push idx dir)
	springTransfers =
	    case force of
		 Push idx dir -> [ Push idx' dir |
			c@(Connection (ridx,_) (eidx,_) (Spring sdir natLen)) <- conns
			, let curLen = connectionLength st c
			, let root = idx == ridx
			, let end = idx == eidx
			, root || end
			, let idx' = if root then eidx else ridx
			, let pull = (root && dir == neg sdir) || (end && dir == sdir)
			, let push = (root && dir == sdir) || (end && dir == neg sdir)
			, (push && curLen <= natLen`div`2) || 
			    (pull && curLen >= natLen*2) ]
		 _ -> []

type AttrChar = (Char, Curses.Attr)
type ColPair = Int
data Glyph = Glyph Char ColPair Curses.Attr
tileChar :: Tile -> AttrChar
a0 = Curses.attr0
bold = Curses.setBold a0 True
tileChar BlockTile = ('#',a0)
tileChar PivotTile = ('o',bold)
tileChar (ArmTile dir principal) = let
    cdir = canonDir dir
    c | cdir == hu = '-'
      | cdir == hv = '\\'
      | cdir == hw = '/'
    a = if principal then bold else a0
    in (c,a)
tileChar HookTile = ('@',bold)
tileChar (WrenchTile moving) = ('*',if moving then bold else a0)
tileChar (BallTile moving) = ('O',if moving then bold else a0)
tileChar (SpringTile Relaxed) = ('S',a0)
tileChar (SpringTile Compressed) = ('$',bold)
tileChar (SpringTile Stretched) = ('s',bold)

ownedTileGlyph :: PieceColouring -> OwnedTile -> Glyph
ownedTileGlyph colouring (owner,t) =
    let (ch,attr) = tileChar t
	pair = case Map.lookup owner colouring of
		    Nothing -> 0
		    Just n -> n+1
    in Glyph ch pair attr

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

drawState :: [Curses.Pair] -> GameState -> HexPos -> IO ()
drawState pairs st centre =
    let board = stateBoard st
	colouring = boardColouring board coloured
	coloured = [ idx |
	    (idx,pp@(PlacedPiece pos p)) <- enumVec $ placedPieces st
	    , case p of (Ball _) -> False
			(Wrench _) -> False
			(Hook _) -> False
			(Pivot _) -> True
			(Block _) -> not $ null $ springsAtIdx st idx ]
    in do
	(h,w) <- Curses.scrSize
	let scrCentre = CVec (h`div`2) (w`div`2)
	Curses.erase
	sequence_ [ mvAddGlyph cpairs cc glyph | 
	    (pos,glyph) <- Map.toList $ Map.map (ownedTileGlyph colouring) board
	    , cc <- [ scrCentre <+> (hexVec2CVec (pos <-> centre)) ]
	    ]
	Curses.refresh

-- testBoard = Map.insert (PHS zero) (Entity (Spring True) zero) Map.empty
--testBoard = Map.fromList [ (PHS ((-n) <*> hw), if n<2 then Entity (Spring False) []
--					       else if n<6 then Entity Block []
--					       else Entity (Wrench hw) [])
--					       | n <- [0..6::Int] ]

orig = PHS zero
ballState = GameState (Vector.fromList [
		PlacedPiece orig (Block [ zero, hu ])
		, PlacedPiece (((8::Int) <*> hv) <+> hu <+> orig) (Block [zero, neg hu])
		, PlacedPiece (((6::Int) <*> hv) <+> orig) (Ball zero)
		, PlacedPiece (((2::Int) <*> hv) <+> orig) (Ball zero)
	    ]) [
		Connection (0, hu) (1,zero) (Spring hv 4)
	    ] 
wrenchState = GameState (Vector.fromList [
		PlacedPiece orig (Block [ zero, hu ])
		, PlacedPiece (((5::Int) <*> hv) <+> hu <+> orig) (Block [zero, neg hu])
		, PlacedPiece (((1::Int) <*> hv) <+> orig) (Wrench hv)
	    ]) [
		Connection (0, hu) (1,zero) (Spring hv 2)
	    ] 
cogState = GameState (Vector.fromList [
		PlacedPiece orig (Block [ zero ])
		, PlacedPiece (10 <*> hw <+> orig) (Block $ concat $
		    [ map (((2*n) <*> hw) <+>) [zero, hu, hw, hw<+>hw] | n <- [0..5] ])
		, PlacedPiece (hu <+> hu <+> (14 <*> hw) <+> orig) (Pivot [ hu,hv,hw ] )
		, PlacedPiece ((3<*>hu) <+> (20<*>hw) <+> orig) (Ball $ neg hw)
		, PlacedPiece ((3<*>hu) <+> (17<*>hw) <+> orig) (Ball $ neg hw)
		, PlacedPiece ((3<*>hu) <+> (10<*>hw) <+> orig) (Wrench $ hw)
	    ]) [
		Connection (0, zero) (1,zero) (Spring hw 5)
	    ] 

-- From Curses.CursesHelper:
-- | Converts a list of 'Curses.Color' pairs (foreground color and
--   background color) into the curses representation 'Curses.Pair'.
colorsToPairs :: [(Curses.Color, Curses.Color)] -> IO [Curses.Pair]
colorsToPairs cs =
    do p <- Curses.colorPairs
       let nColors = length cs
           blackWhite = p < nColors
       if blackWhite
          then trace ("Terminal does not support enough colors. Number of " ++
                      " colors requested: " ++ show nColors ++
                      ". Number of colors supported: " ++ show p)
                 return $ take nColors (repeat (Curses.Pair 0))
          else mapM toPairs (zip [1..] cs)
     where toPairs (n, (fg, bg)) =
               let p = Curses.Pair n
               in do Curses.initPair p fg bg
                     return p


--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test = do
    CursesH.start 
    pairs <- colorsToPairs [ (f, CursesH.black) | f <-
	[ CursesH.white, CursesH.red, CursesH.green, CursesH.yellow, CursesH.blue] ] 
    showDynamics pairs cogState (PHS zero)
    CursesH.end
    where showDynamics pairs s v =
	    do drawState pairs s v
	       k <- CursesH.getKey Curses.refresh
	       if k /= Curses.KeyChar 'q' then showDynamics pairs (physicsTick s nullMove) v
					  else return ()

main = test
