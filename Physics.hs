module Physics where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as Vector
import Data.List (intersect, union, (\\), elemIndices, nub)
import Data.Maybe
import Control.Monad.State
import Control.Monad.Writer
import Data.Vector (Vector, (!), (!?), (//))

import Hex
import GameState

-- |TorqueDir: Int of absolute value <= 1
type TorqueDir = Int

-- |Crappy terminology: 'force' encompasses both usual directional forces and
-- torques; we use 'push' for the former.
data Force = Push PieceIdx HexDir | Torque PieceIdx TorqueDir
    deriving (Eq, Ord, Show)

forceIdx force = case force of (Push idx _) -> idx
			       (Torque idx _) -> idx
isPush (Push _ _) = True
isPush _ = False
isTorque = not . isPush
forceIsNull (Push _ dir) = dir == zero
forceIsNull (Torque _ dir) = dir == 0
getForcedpp :: GameState -> Force -> PlacedPiece
getForcedpp s f = getpp s (forceIdx f)

type Forces = Vector.Vector Force

-- |PlayerMove: if not NullPM, the direction should be non-zero
data PlayerMove = NullPM | HookPush HexDir | HookTorque TorqueDir | WrenchPush HexDir
    deriving (Eq, Ord, Show)

-- |Alert: for passing information about physics processing to the UI
data Alert = AlertCollision HexPos | AlertBlockedForce PieceIdx Force
    deriving (Eq, Ord, Show)

toolForces :: GameState -> PlayerMove -> Forces
toolForces st pm = Vector.fromList $
    [ Push widx mom | (widx, PlacedPiece _ (Wrench mom)) <- epps ]
    ++ ( case pm of
	HookTorque ht -> [ Torque hidx ht | (hidx, PlacedPiece _ (Hook _ _)) <- epps ]
	_ -> [ Push hidx hp | (hidx, PlacedPiece _ (Hook _ _)) <- epps ]
	)
    where
	epps = enumVec $ placedPieces st
	hp = case pm of {HookPush hp -> hp; _ -> zero}


envForces :: GameState -> Forces
envForces st@(GameState plPieces conns) =
    Vector.fromList $ 
	[ Push endIdx dir | c@(Connection _ (endIdx, _) (Spring outDir natLen)) <- conns
			    , let curLen = connectionLength st c
			    , natLen /= curLen
			    , let dir = if natLen > curLen then outDir else neg outDir ]

setTools :: PlayerMove -> GameState -> GameState
setTools pm st =
    let hf = case pm of
	    HookTorque dir -> TorqueHF dir
	    HookPush v -> PushHF v
	    _ -> NullHF
    in adjustPieces (\p -> case p of
		Hook arm _ -> Hook arm hf
		Wrench mom -> Wrench $ if mom == zero
		    then case pm of {WrenchPush dir -> dir; _ -> zero}
		    else mom
		_ -> p) st

physicsTick :: PlayerMove -> GameState -> Writer [Alert] GameState
physicsTick pm st =
    let wrenchesStopped st' =
	    and [ mom == zero |
		(PlacedPiece _ (Wrench mom)) <- Vector.toList $ placedPieces st' ]
	doTools st = let st' = setTools pm st
	    in setTools NullPM `liftM` resolveForces (toolForces st' pm) Vector.empty st'
    in do
	let (st', alerts1) = runWriter $ doTools st
	st' <- if and [case pm of {WrenchPush _ -> True; _ -> False}
		, or [ mom /= zero && mom' == zero
		    | (idx, PlacedPiece _ (Wrench mom)) <- enumVec $ placedPieces st
		    , let PlacedPiece _ (Wrench mom') = getpp st' idx ] ]
	    then -- we're pushing a moving wrench and it got blocked. Try again.
		doTools st'
	    else do
		tell alerts1
		return st'
	resolveForces Vector.empty (envForces st') st'

type Source = Int
data SourcedForce = SForce Source Force Bool Bool
    deriving (Eq, Ord, Show)
resolveForces :: Forces -> Forces -> GameState -> Writer [Alert] GameState
resolveForces plForces eForces st =
    let resolveForce :: SourcedForce -> StateT ([SourcedForce], [Source]) (Writer [Alert]) ()
	resolveForce sforce@(SForce s f isPlForce isPlSource) = do
	    (sforces,blockedsources) <- get
	    blockInfo <- lift $ checkBlocks st sforces sforce
	    let (propagated, moving, blocks) = case blockInfo of
		    Unmoving -> ([], False, [s])  -- nip
		    Moving blocks ->
			let propagated = [ SForce s f' False isPlSource | f' <- propagateForce st f isPlSource ] \\ sforces
			in (propagated, True, blocks)
	    put (if moving then sforce:sforces else sforces, blocks ++ blockedsources)
	    sequence_ $ map resolveForce propagated
	forces = plForces Vector.++ eForces
	sforces = [ SForce s f isPl isPl | (s,f) <- enumVec forces, let isPl = s < Vector.length plForces ]
	stopWrench idx = setPiece idx (Wrench zero)
	stopBlockedWrenches blocked st' = foldr stopWrench st' [
	    forceIdx $ forces!s | s <- blocked
	    , (Wrench _) <- [placedPiece $ getForcedpp st' $ forces!s] ]
    in do --prettyTrace (st, sforces, sforces', blocked, unblocked) $
	(sforces', blockedsfs) <- (sequence_ (map resolveForce sforces)) `execStateT` ([],[])
	let unblocked = nub [ force | SForce s force _ _ <- sforces', not $ s `elem` blockedsfs ]
	tell $ nub [ AlertBlockedForce (forceIdx $ forces!s) force | SForce s force _ _ <- sforces', s `elem` blockedsfs ]
	return $ stopBlockedWrenches blockedsfs $ foldr applyForce st unblocked
    
applyForce :: Force -> GameState -> GameState
applyForce f s =
    let idx = forceIdx f
	pp' = applyForceTo (getpp s idx) f
	pp'' = case (placedPiece pp',f) of
		   ( (Wrench _), (Push _ dir) ) -> pp' {placedPiece = (Wrench dir)}
		   _ -> pp'
	in 
	s { placedPieces = (placedPieces s) // [(idx, pp'')] }

applyForceTo :: PlacedPiece -> Force -> PlacedPiece
applyForceTo (PlacedPiece pos piece) (Push _ dir) =
    PlacedPiece (dir <+> pos) piece
applyForceTo (PlacedPiece pos (Pivot arms)) (Torque _ dir) =
    PlacedPiece pos (Pivot $ map (rotate dir) arms)
applyForceTo (PlacedPiece pos (Hook arm hf)) (Torque _ dir) =
    PlacedPiece pos (Hook (rotate dir arm) hf)
applyForceTo pp _ = pp

data BlocksReturn = Unmoving | Moving [Source]
checkBlocks :: GameState -> [SourcedForce] -> SourcedForce -> Writer [Alert] BlocksReturn
checkBlocks st@(GameState pps conns) sforces sforce@(SForce source force isPlForce isPlSource) =
    let idx = forceIdx force
	pp = getForcedpp st force
	pp' = applyForceTo pp force
        mismatches = [ s | SForce s force' _ _ <- sforces, idx == forceIdx force', force /= force' ]
	(pieceCollisions, collisionPoints) = unzip [ (s,pos) |
	    SForce s force' _ _ <- sforces
	    , let idx' = forceIdx force'
	    , idx /= idx'
	    , not $ forceIsNull force'
	    , let st' = applyForce force $ applyForce force' st
	    , pos <- collisions st' idx idx' ]
	blocks = mismatches ++ nub pieceCollisions
    in do
	if pieceResists st force (not isPlSource) (not isPlForce)
	    then return $ Unmoving
	    else if null blocks
		then return $ Moving []
		else do tell $ map AlertCollision collisionPoints
			return $ Moving $ source:blocks

-- A force on a piece which resists it is immediately blocked
pieceResists :: GameState -> Force -> Bool -> Bool -> Bool 
pieceResists st@(GameState pps conns) force toolsFixed fixedFixed =
    let idx = forceIdx force
	PlacedPiece pos piece = getpp st idx
        springs = springsEndAtIdx st idx
	-- |blockingSprings: a spring resists any push not along its line
	blockingSprings = [ c |
		    c@(Connection (ridx,_) (eidx,_) (Spring sdir _)) <- conns
		    , idx `elem` [ridx,eidx]
		    , (Push _ pdir) <- [force]
		    , not (pdir `elem` [sdir, neg sdir]) ]
	fixed = fixedFixed && case piece of
		     (Pivot _) -> isPush force
		     (Block _) -> null springs
		     (Wrench mom) -> toolsFixed || case force of
			    Push _ v -> v /= mom
			    _ -> True
		     (Hook _ hf) -> toolsFixed || case force of 
			    Push _ v -> hf /= PushHF v
			    Torque _ dir -> hf /= TorqueHF dir
		     _ -> False
	-- |mediatedResistance: balls and fully compressed/extended springs
	-- "immediately transmit" a push, so resist it if the transmitted
	-- force is resisted.
	mediatedResistance = Just True == do
	    Push _ dir <- Just force
	    cpos <- do { guard $ isBall piece; return $ dir<+>pos}
		`mplus` listToMaybe [ locusPos st root
		    | c@(Connection root _ (Spring sdir _)) <- springs
		    , or [ dir == sdir && springFullyExtended st c
			, dir == neg sdir && springFullyCompressed st c]
		    ]
	    (idx',_) <- Map.lookup cpos $ stateBoard st
	    return $ pieceResists st (transmittedForce st idx' cpos dir) toolsFixed fixedFixed

    in fixed || not (null blockingSprings) || mediatedResistance

-- |transmittedForce: convert pushes into torques as appropriate
transmittedForce :: GameState -> Source -> HexPos -> HexDir -> Force
transmittedForce st idx cpos dir =
    let pp@(PlacedPiece pos piece) = getpp st idx
	rpos = cpos <-> placedPos pp
	armPush = case
		(dir `hexDot` ((rotate 1 rpos) <-> rpos)) `compare`
		(dir `hexDot` ((rotate (-1) rpos) <-> rpos)) of
	    GT -> Torque idx 1
	    LT -> Torque idx $ -1
	    EQ -> Push idx dir
    in case piece of
	    Pivot _ -> armPush
	    Hook _ (TorqueHF _) -> armPush
	    _ -> (Push idx dir)

-- |propagateForce: return forces a force causes via bumps and fully
-- compressed/extended springs
propagateForce :: GameState -> Force -> Bool -> [Force]
propagateForce st@(GameState plPieces conns) force isPlSource =
    bumps ++ springTransmissions
    where
	idx = forceIdx force
	bumps = [ transmittedForce st idx' cpos dir |
	    idx' <- ppidxs st
	    , idx' /= idx
	    , cpos <- collisions (applyForce force st) idx idx'
	    , let dir = case force of
		    Push _ dir -> dir
		    Torque _ dir -> if pieceResists st (transmittedForce st idx' cpos push) (not isPlSource) True
					 then claw else push
			where push = arm <-> rotate (-dir) arm
			      claw = rotate dir arm <-> arm
			      arm = cpos <-> placedPos (getpp st idx) ]
	springTransmissions =
	    case force of
		 Push idx dir -> [ Push idx' dir |
			c@(Connection (ridx,_) (eidx,_) (Spring sdir _)) <- conns
			, let root = idx == ridx
			, let end = idx == eidx
			, root || end
			, let idx' = if root then eidx else ridx
			, let pull = (root && dir == neg sdir) || (end && dir == sdir)
			, let push = (root && dir == sdir) || (end && dir == neg sdir)
			, (push && if isPlSource then springFullyCompressed st c else not $ springExtended st c) ||
			    (pull && if isPlSource then springFullyExtended st c else not $ springCompressed st c) ]
		 _ -> []
