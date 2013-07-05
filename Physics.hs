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

data PlayerMove = PlayerMove { hookPush :: HexDir, hookTorque :: TorqueDir, wrenchPush :: HexDir }
    deriving (Eq, Ord, Show)
nullMove = PlayerMove zero 0 zero

-- |Alert: for passing information about physics processing to the UI
data Alert = AlertCollision HexPos | AlertBlockedForce Force
    deriving (Eq, Ord, Show)

-- | player move results in a wrench push and either a non-zero hook torque or
-- a hook push. Pushes can be zero.
toolForces :: GameState -> PlayerMove -> Forces
toolForces st (PlayerMove hp ht wp) = Vector.fromList $
    [ Push widx mom | (widx, PlacedPiece _ (Wrench mom)) <- epps ]
    ++ ( if ht == 0
	     then [ Push hidx hp | (hidx, PlacedPiece _ (Hook _ _)) <- epps ]
	     else [ Torque hidx ht | (hidx, PlacedPiece _ (Hook _ _)) <- epps ] )
    where epps = enumVec $ placedPieces st

envForces :: GameState -> Forces
envForces st@(GameState plPieces conns) =
    Vector.fromList $ 
	[ Push endIdx dir | c@(Connection _ (endIdx, _) (Spring outDir natLen)) <- conns
			    , let curLen = connectionLength st c
			    , natLen /= curLen
			    , let dir = if natLen > curLen then outDir else neg outDir ]

setTools :: PlayerMove -> GameState -> GameState
setTools move st =
    let stiffness | hookTorque move /= 0 = PushStiff
	    | hookPush move /= zero = TorqueStiff
	    | otherwise = ForceStiff
    in adjustPieces (\p -> case p of
		Hook arm _ -> Hook arm stiffness
		Wrench mom -> Wrench $ if mom == zero then wrenchPush move else mom
		_ -> p) st
setHookStiff :: GameState -> GameState
setHookStiff = adjustPieces (\p -> case p of
		Hook arm _ -> Hook arm ForceStiff
		_ -> p)

physicsTick :: PlayerMove -> GameState -> Writer [Alert] GameState
physicsTick move st =
    let wrenchMomentum st' =
	    head [ mom |
		(PlacedPiece _ (Wrench mom)) <- Vector.toList $ placedPieces st' ]
	initWrenchMomentum = wrenchMomentum st
    in do
	st <- let st' = setTools move st
		  in resolveForces (toolForces st' move) Vector.empty st'
	st <- if and [wrenchPush move /= zero, initWrenchMomentum /= zero
			, wrenchMomentum st == zero]
		    then let st' = setTools move st
			     in resolveForces (toolForces st' move) Vector.empty st'
		    else return st
	st <- return $ setHookStiff st
	let efs = envForces st
	st <- resolveForces Vector.empty efs st
	return st

type Source = Int
type SourcedForce = (Source,Force)
resolveForces :: Forces -> Forces -> GameState -> Writer [Alert] GameState
resolveForces plForces envForces st =
    let
	resolveForce :: SourcedForce -> StateT ([SourcedForce], [Source]) (Writer [Alert]) ()
	resolveForce sforce@(s,f) = do
	    (sforces,blockedsources) <- get
	    blockInfo <- lift $ checkBlocks st sforces plsforces sforce
	    let (propagated, moving, blocks) = case blockInfo of
		    Unmoving -> ([], False, [s])  -- nip
		    Moving blocks ->
			let propagated = [ (s, f') | f' <- propagateForce st f ] \\ sforces
			in (propagated, True, blocks)
	    put (if moving then sforce:sforces else sforces, blocks ++ blockedsources)
	    sequence_ $ map resolveForce propagated
	forces = plForces Vector.++ envForces
	sforces = enumVec forces
	plsforces = enumVec plForces
	-- stop wrenches which are blocked
	stopWrench idx = setPiece idx (Wrench zero)
	stopWrenches blocked st' = foldr stopWrench st' [
	    forceIdx $ forces!s | s <- blocked
	    , (Wrench _) <- [placedPiece $ getForcedpp st' $ forces!s] ]
    in do --prettyTrace (st, sforces, sforces', blocked, unblocked) $
	(sforces', blockedsfs) <- (sequence_ (map resolveForce sforces)) `execStateT` ([],[])
	let blocked = nub [ force | (s,force) <- sforces', s `elem` blockedsfs ]
	let unblocked = nub [ force | (s,force) <- sforces', not $ s `elem` blockedsfs ]
	tell $ map AlertBlockedForce blocked
	return $ stopWrenches blockedsfs $ foldr applyForce st unblocked
    
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
applyForceTo (PlacedPiece pos (Hook arm tstiff)) (Torque _ dir) =
    PlacedPiece pos (Hook (rotate dir arm) tstiff)
applyForceTo pp _ = pp

data BlocksReturn = Unmoving | Moving [Source]
checkBlocks :: GameState -> [SourcedForce] -> [SourcedForce] -> SourcedForce -> Writer [Alert] BlocksReturn
checkBlocks st@(GameState pps conns) sforces plsforces sforce@(source, force) =
    let idx = forceIdx force
	pp@(PlacedPiece pos piece) = (getpp st idx)
	pp' = applyForceTo pp force
        mismatches = [ s | (s,force') <- sforces, idx == forceIdx force', force /= force' ]
	(pieceCollisions, collisionPoints) = unzip [ (s,pos) |
	    (s,force') <- sforces
	    , let idx' = forceIdx force'
	    , idx /= idx'
	    , not $ forceIsNull force'
	    , let st' = applyForce force $ applyForce force' st
	    , pos <- collisions st' idx idx' ]
	blocks = mismatches ++ nub pieceCollisions
	isPlayerSource = source `elem` map fst plsforces
	isPlayerForce = sforce `elem` plsforces
    in do
	if pieceResists st force (not isPlayerSource) (not isPlayerSource) (not isPlayerForce)
	    then return $ Unmoving
	    else if null blocks
		then return $ Moving []
		else do -- when isPlayer $
			tell $ map AlertCollision collisionPoints
			return $ Moving $ source:blocks

pieceResists :: GameState -> Force -> Bool -> Bool -> Bool -> Bool
pieceResists st@(GameState pps conns) force toolsFixed springsResist fixedFixed =
    let idx = forceIdx force
	pp@(PlacedPiece pos piece) = (getpp st idx)
        springs = springsEndAtIdx st idx
	blockingSprings = [ c |
		    c@(Connection (ridx,_) (eidx,_) (Spring sdir natLen)) <- conns
		    , idx `elem` [ridx,eidx]
		    , let curLen = connectionLength st c
		    , (Push _ pdir) <- [force]
		    , not (pdir `elem` [sdir, neg sdir]) || (
			idx == eidx && springsResist &&
			    ( (curLen <= natLen && pdir /= sdir) ||
				(curLen >= natLen && pdir /= neg sdir) ) ) ]
	fixed = fixedFixed && case piece of
		     (Pivot _) -> isPush force
		     (Wrench mom) -> toolsFixed || mom == zero
		     (Hook _ stiffness) ->
			 or [toolsFixed, stiffness == ForceStiff
			    , stiffness == PushStiff && isPush force
			    , stiffness == TorqueStiff && isTorque force]
		     (Block _) -> null springs
		     _ -> False
	mediatedResistance = Just True == do
	    Ball <- Just piece
	    Push _ dir <- Just force
	    let cpos = dir<+>pos
	    (idx',_) <- Map.lookup cpos $ stateBoard st
	    return $ isBall piece && pieceResists st (transferredForce st idx' cpos dir) toolsFixed springsResist fixedFixed

    in fixed || not (null blockingSprings) || mediatedResistance

transferredForce :: GameState -> Source -> HexPos -> HexDir -> Force
transferredForce st idx cpos dir =
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
	    Hook _ PushStiff -> armPush
	    _ -> (Push idx dir)
propagateForce :: GameState -> Force -> [Force]
propagateForce st@(GameState plPieces conns) force = 
    bumps ++ springTransfers
    where
	idx = forceIdx force
	bumps = [ transferredForce st idx' cpos dir |
	    idx' <- ppidxs st
	    , idx' /= idx
	    , cpos <- collisions (applyForce force st) idx idx'
	    , let dir = case force of
		    Push _ dir -> dir
		    Torque _ dir -> if pieceResists st (transferredForce st idx' cpos push) True False True
					 then claw else push
			where push = arm <-> rotate (-dir) arm
			      claw = rotate dir arm <-> arm
			      arm = cpos <-> placedPos (getpp st idx) ]
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
			, (push && curLen <= (natLen+1)`div`2) || 
			    (pull && curLen >= natLen*2) ]
		 _ -> []
