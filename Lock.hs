module Main where

import Hex
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector
import Data.List (intersect, union, (\\), elemIndices, nub)
import Data.Monoid
import Data.Maybe
import Control.Monad.State
import Data.Vector (Vector, (!), (!?), (//))
import qualified Debug.Trace as Trace
import Text.Show.Pretty (ppShow)

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

--data State = State (Tree Connection Entity)
    --deriving (Eq, Ord, Show)

type HexPos = PHS HexVec

data GameState = GameState { statePlacedPieces :: Vector PlacedPiece,
		     stateConnections :: [Connection] }
    deriving (Eq, Ord, Show)

data PlacedPiece = PlacedPiece { placedPos :: HexPos, placedPiece :: Piece }
    deriving (Eq, Ord, Show)

getpp :: GameState -> PieceIdx -> PlacedPiece
getpp st idx = fromJust $ (statePlacedPieces st) !? idx

setPiece :: GameState -> PieceIdx -> Piece -> GameState
setPiece st@(GameState pps _) idx p = 
    st { statePlacedPieces = pps //
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
connectionLength state (Connection root end _) =
    let rootPos = locusPos state root
	endPos = locusPos state end
    in hexLen (endPos <-> rootPos) - 1

data Link = Free { freePos :: HexVec }
	  | Spring { springDir :: HexDir, springNatLength :: Int }
    deriving (Eq, Ord, Show)

data SpringExtension = Relaxed | Compressed | Stretched
    deriving (Eq, Ord, Show)

data Tile = BlockTile | PivotTile | ArmTile HexDir | HookTile | WrenchTile
	  | BallTile | SpringTile SpringExtension
    deriving (Eq, Ord, Show)

type GameBoard = Map HexPos Tile

stateBoard :: GameState -> GameBoard
stateBoard state@(GameState plPieces connections) =
    Map.union (Map.unions (map plPieceBoard (Vector.toList plPieces)))
	  (Map.unions (map (connectionBoard state) connections))

plPieceBoard :: PlacedPiece -> GameBoard
plPieceBoard (PlacedPiece pos (Block pattern)) =
    Map.fromList [ (rel <+> pos, BlockTile) | rel <- pattern ]
plPieceBoard (PlacedPiece pos (Pivot arms)) =
    Map.fromList $ (pos, PivotTile) : [ (rel <+> pos, ArmTile rel) | rel <- arms ]
plPieceBoard (PlacedPiece pos (Hook arm)) =
    Map.fromList $ (pos, HookTile) : [ (arm <+> pos, ArmTile arm) ]
plPieceBoard (PlacedPiece pos (Wrench _)) = Map.singleton pos WrenchTile
plPieceBoard (PlacedPiece pos (Ball _)) = Map.singleton pos BallTile

plPieceFootprint :: PlacedPiece -> [HexPos]
plPieceFootprint = Map.keys . plPieceBoard

-- TODO: use bounding boxes to optimise collision detection?
collideEntities :: PlacedPiece -> PlacedPiece -> [HexPos]
collideEntities pp pp' =
    intersect (plPieceFootprint pp) (plPieceFootprint pp')
entitiesIntersect :: PlacedPiece -> PlacedPiece -> Bool
entitiesIntersect pp pp' = not $ null $ collideEntities pp pp'

connectionBoard :: GameState -> Connection -> GameBoard
connectionBoard state (Connection root end (Spring dir natLen)) =
    let rootPos = locusPos state root
	endPos = locusPos state end
	curLen = hexLen (endPos <-> rootPos) - 1
    in Map.fromList $
	[ ((d <*> dir) <+> rootPos, SpringTile extension) 
	    | d <- [1..curLen],
	    let extension | d <= natLen - curLen = Compressed
			  | curLen-d < 2*(curLen - natLen) = Stretched
			  | otherwise = Relaxed ]
connectionBoard _ _ = Map.empty

connectionFootPrint :: GameState -> Connection -> [HexPos]
connectionFootPrint s c = Map.keys $ connectionBoard s c

entityIntersectsConnection :: GameState -> PlacedPiece -> Connection -> Bool
entityIntersectsConnection s pp c =
    not $ null $ intersect (plPieceFootprint pp) (connectionFootPrint s c)

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
toolForces st move = Vector.fromList [ Push idx dir |
	(idx, PlacedPiece _ (Wrench mom)) <- enumVec $ statePlacedPieces st,
	let dir = if mom == zero then wrenchPush move else mom ]

envForces :: GameState -> Forces
envForces state@(GameState plPieces connections) = Vector.fromList $
	[ Push endIdx dir | c@(Connection _ (endIdx, _) (Spring outDir natLen)) <- connections,
			    let curLen = connectionLength state c,
			    natLen /= curLen,
			    let dir = if natLen > curLen then outDir else neg outDir ]
	++ [ Push idx momentum | (idx,(PlacedPiece _ (Ball momentum))) <- enumVec plPieces,
				 momentum /= zero ]

physicsTick :: GameState -> PlayerMove -> GameState
physicsTick state move =
    let state' = resolveForces state (toolForces state move) True
	in resolveForces state' (envForces state') False

type Source = Int
type SourcedForce = (Source,Force)
resolveForces :: GameState -> Forces -> Bool -> GameState
resolveForces state forces isPlayer =
    let
	resolveForce :: SourcedForce -> State ([SourcedForce], [Source]) ()
	resolveForce sforce = do
	    (sforces,blockedsources) <- get
	    let blocks = checkBlocks state sforces sforce isPlayer
	    let propagated = [ (fst sforce, f) | f <- propagateForce state (snd sforce) ] \\ sforces
	    put (propagated ++ (sforce:sforces), blocks ++ blockedsources)
	    sequence_ $ map resolveForce propagated
	sforces = enumVec forces
	(sforces', blocked) = (sequence_ (map resolveForce sforces)) `execState` ([],[])
	-- stop balls which are blocked or cause a propagated force
	propagatingBallForces = 
	    [ s | s <- [0 .. (Vector.length forces)-1],
		(Ball _) <- [placedPiece $ getForcedpp state $ forces!s],
		drop 1 (elemIndices s (map fst sforces')) /= [] ]
	stopBall st' idx = setPiece st' idx (Ball zero)
	stopBalls st' = foldl stopBall st' [
	    forceIdx $ forces!s | s <- blocked `union` propagatingBallForces,
		(Ball _) <- [placedPiece $ getForcedpp st' $ forces!s] ]
	-- stop wrenches which are blocked
	stopWrench st' idx = setPiece st' idx (Wrench zero)
	stopWrenches st' = foldl stopWrench st' [
	    forceIdx $ forces!s | s <- blocked,
		(Wrench _) <- [placedPiece $ getForcedpp st' $ forces!s] ]
	unblocked = nub [ force | (s,force) <- sforces', not $ s `elem` blocked ] \\
	    [ forces!s | s <- propagatingBallForces ]
	applyForces st' = foldl applyForce st' unblocked
    in --prettyTrace (state, sforces, sforces', blocked, unblocked) $
	stopBalls $ stopWrenches $ applyForces state
    
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
	s { statePlacedPieces = (statePlacedPieces s) // [(idx, pp'')] }
applyForceTo :: PlacedPiece -> Force -> PlacedPiece
applyForceTo (PlacedPiece pos piece) (Push _ dir) =
    PlacedPiece (dir <+> pos) piece
applyForceTo (PlacedPiece pos (Pivot arms)) (Torque _ dir) =
    PlacedPiece pos (Pivot $ map (rotate dir) arms)
applyForceTo (PlacedPiece pos (Hook arm)) (Torque _ dir) =
    PlacedPiece pos (Hook $ rotate dir arm)

checkBlocks :: GameState -> [SourcedForce] -> SourcedForce -> Bool -> [Source]
checkBlocks state@(GameState plPieces connections) sforces sforce@(source, force) isPlayer =
    let idx = forceIdx force
	pp@(PlacedPiece pos piece) = (getpp state idx)
	pp' = applyForceTo pp force
        springs = [ c | c@(Connection _ (eidx, _) (Spring _ _)) <- connections, eidx==idx ]
	blockingSprings = [ c |
		    c@(Connection _ (eidx, _) (Spring sdir natLen)) <- springs,
		    idx == eidx,
		    let curLen = connectionLength state c,
		    (Push _ pdir) <- [force],
		    not (pdir `elem` [sdir, neg sdir]) || (
			not isPlayer &&
			    ( (curLen < natLen && pdir /= sdir) ||
				(curLen > natLen && pdir /= neg sdir) ) ) ]
	linkCollisions = [ c | c <- connections,
	    (fst . connectionEnd) c /= idx,
	    (fst . connectionRoot) c /= idx,
	    entityIntersectsConnection state pp' c ]
	fixed = case piece of
		     (Pivot _) -> case force of {(Push _ _) -> True; _ -> False}
		     (Wrench _) -> not isPlayer
		     (Hook _) -> not isPlayer
		     (Block _) -> null springs
		     _ -> False
        mismatches = [ s | (s,force') <- sforces, idx == forceIdx force', force /= force' ]
	entityCollisions = [ s |
	    (s,force') <- sforces,
	    let idx' = forceIdx force',
	    idx /= idx',
	    entitiesIntersect pp'
		(applyForceTo (getpp state idx') force')]
	blocks = mismatches ++ entityCollisions
	blocked = or [not $ null blocks,
	    not $ null blockingSprings,
	    not $ null linkCollisions,
	    fixed]
    in if blocked then source:blocks else blocks

propagateForce :: GameState -> Force -> [Force]
propagateForce state@(GameState plPieces conns) force = 
    bumps ++ springTransfers
    where
	bumps = [ transferredForce idx' (cpos <-> (placedPos pp')) dir |
	    let pp = applyForceTo (getForcedpp state force) force,
	    (idx',pp') <- enumVec plPieces,
	    idx' /= forceIdx force,
	    cpos <- collideEntities pp pp',
	    let dir = case force of
		    (Push _ dir) -> dir
		    (Torque _ dir) -> arm <-> rotate (-dir) arm
			where arm = cpos <-> placedPos pp ]
	transferredForce :: Source -> HexVec -> HexDir -> Force
	transferredForce idx rpos dir =
	    let pp@(PlacedPiece pos piece) = getpp state idx
		armPush
		    | dir `hexDot` ((rotate 1 rpos) <-> rpos) > 0 = Torque idx 1
		    | dir `hexDot` ((rotate (-1) rpos) <-> rpos) > 0 = Torque idx (-1)
		    | otherwise = Push idx dir
	    in
		case piece of
		    (Pivot _) -> armPush
		    (Hook _) -> armPush
		    _ -> (Push idx dir)
	springTransfers =
	    case force of
		 Push idx dir -> [ Push idx' dir |
			c@(Connection (ridx,_) (eidx,_) (Spring sdir natLen)) <- conns,
			let curLen = connectionLength state c,
			let root = idx == ridx,
			let end = idx == eidx,
			root || end,
			let idx' = if root then eidx else ridx,
			let pull = (root && dir == neg sdir) || (end && dir == sdir),
			let push = (root && dir == sdir) || (end && dir == neg sdir),
			(push && curLen <= natLen`div`2) || 
			    (pull && curLen >= natLen*2) ]
		 _ -> []

tileChar :: Tile -> Char
tileChar BlockTile = '#'
tileChar PivotTile = 'o'
tileChar (ArmTile dir) | cdir == hu = '-'
		       | cdir == hv = '\\'
		       | cdir == hw = '/'
    where cdir = canonDir dir
tileChar HookTile = '@'
tileChar WrenchTile = '*'
tileChar BallTile = 'O'
tileChar (SpringTile Relaxed) = 'S'
tileChar (SpringTile Compressed) = '$'
tileChar (SpringTile Stretched) = 's'

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
mvAddCh (CVec y x) c = Curses.move y x >> Curses.wAddStr Curses.stdScr [c]

drawState :: GameState -> HexPos -> IO ()
drawState state centre =
    do	(h,w) <- Curses.scrSize
	let scrCentre = CVec (h`div`2) (w`div`2)
	Curses.erase
	sequence_ [ mvAddCh cc ch | 
		    (pos,ch) <- Map.toList $ Map.map tileChar (stateBoard state),
		    cc <- [ scrCentre <+> (hexVec2CVec (pos <-> centre)) ]
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
		, PlacedPiece (hu <+> hu <+> (10 <*> hw) <+> orig) (Pivot [ hu,hv,hw ] )
	    ]) [
		Connection (0, zero) (1,zero) (Spring hw 5)
	    ] 

--test = CursesH.start >> drawBoard testBoard (PHS zero) >> CursesH.getKey (return ()) >> CursesH.end
test = do
    CursesH.start 
    showDynamics cogState (PHS zero)
    CursesH.end
    where showDynamics s c =
	    do drawState s c
	       k <- CursesH.getKey Curses.refresh
	       if k /= Curses.KeyChar 'q' then showDynamics (physicsTick s nullMove) c
					  else return ()

main = test
