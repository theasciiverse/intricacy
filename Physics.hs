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

-- TorqueDir: Int of absolute value <= 1
type TorqueDir = Int

-- Crappy terminology: 'force' encompasses both usual directional forces and
-- torques; we use 'push' for the former.
data Force = Push PieceIdx HexDir | Torque PieceIdx TorqueDir
    deriving (Eq, Ord, Show)
forceIdx force = case force of (Push idx _) -> idx
			       (Torque idx _) -> idx
isPush force = case force of (Push _ _ ) -> True; _ -> False
isTorque force = case force of (Torque _ _ ) -> True; _ -> False
forceIsNull force = case force of
    Push _ dir -> dir == zero
    Torque _ dir -> dir == 0
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
	, let dir = if mom == zero then wp else mom ]
    -- ++ ( if hp == zero then [] else
	-- [ Push hidx hp | (hidx, PlacedPiece _ (Hook _ _)) <- epps ] )
    ++ ( if ht == 0
	     then [ Push hidx hp | (hidx, PlacedPiece _ (Hook _ _)) <- epps ]
	     else [ Torque hidx ht | (hidx, PlacedPiece _ (Hook _ _)) <- epps ] )
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

setTorqueStiffness :: PlayerMove -> GameState -> GameState
setTorqueStiffness move st =
    foldr (\(idx,hook) -> setPiece idx $ Hook (hookArm hook) (hookTorque move == 0))
	st [ (idx,hook) | (idx,PlacedPiece _ hook@(Hook _ _)) <- enumVec $ placedPieces st ]

physicsTick :: PlayerMove -> GameState -> Writer [Alert] GameState
physicsTick move st =
    do
	st <- resolveForces (toolForces st move) True $ setTorqueStiffness move st
	let (stoppedBalls, forces) = envForces st
	st <- resolveForces forces False st
	let stopBall st'' idx = setPiece idx (Ball zero) st'' 
	let stopBalls st'' = foldl stopBall st'' stoppedBalls
	return $ stopBalls st

type Source = Int
type SourcedForce = (Source,Force)
resolveForces :: Forces -> Bool -> GameState -> Writer [Alert] GameState
resolveForces forces isPlayer st =
    let
	resolveForce :: SourcedForce -> StateT ([SourcedForce], [Source]) (Writer [Alert]) ()
	resolveForce sforce@(s,f) = do
	    (sforces,blockedsources) <- get
	    blockInfo <- lift $ checkBlocks st sforces sforce isPlayer
	    let (propagated, moving, blocks) = case blockInfo of
		    Unmoving -> ([], False, [s])  -- nip
		    Moving blocks ->
			let propagated = [ (s, f') | f' <- propagateForce st f ] \\ sforces
			in (propagated, True, blocks)
	    put (if moving then sforce:sforces else sforces, blocks ++ blockedsources)
	    sequence_ $ map resolveForce propagated
	sforces = enumVec forces
	-- stop wrenches which are blocked
	stopWrench idx = setPiece idx (Wrench zero)
	stopWrenches blocked st' = foldr stopWrench st' [
	    forceIdx $ forces!s | s <- blocked
	    , (Wrench _) <- [placedPiece $ getForcedpp st' $ forces!s] ]
    in do --prettyTrace (st, sforces, sforces', blocked, unblocked) $
	(sforces', blocked) <- (sequence_ (map resolveForce sforces)) `execStateT` ([],[])
	let unblocked = nub [ force | (s,force) <- sforces', not $ s `elem` blocked ]
	return $ stopWrenches blocked $ foldr applyForce st unblocked
    
applyForce :: Force -> GameState -> GameState
applyForce f s =
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
applyForceTo (PlacedPiece pos (Hook arm tstiff)) (Torque _ dir) =
    PlacedPiece pos (Hook (rotate dir arm) tstiff)
applyForceTo pp _ = pp

data BlocksReturn = Unmoving | Moving [Source]
checkBlocks :: GameState -> [SourcedForce] -> SourcedForce -> Bool -> Writer [Alert] BlocksReturn
checkBlocks st@(GameState plPieces conns) sforces sforce@(source, force) isPlayer =
    let idx = forceIdx force
	pp@(PlacedPiece pos piece) = (getpp st idx)
	pp' = applyForceTo pp force
        springs = springsEndAtIdx st idx
	blockingSprings = [ c |
		    c@(Connection (ridx,_) (eidx,_) (Spring sdir natLen)) <- conns
		    , idx `elem` [ridx,eidx]
		    , let curLen = connectionLength st c
		    , (Push _ pdir) <- [force]
		    , not (pdir `elem` [sdir, neg sdir]) || (
			idx == eidx && not isPlayer &&
			    ( (curLen <= natLen && pdir /= sdir) ||
				(curLen >= natLen && pdir /= neg sdir) ) ) ]
	linkCollisions = [ c | c <- conns
	    , (fst . connectionEnd) c /= idx
	    , (fst . connectionRoot) c /= idx
	    , pieceIntersectsConnection st pp' c ]
	fixed = case piece of
		     (Pivot _) -> case force of {(Push _ _) -> True; _ -> False}
		     (Wrench _) -> not isPlayer
		     (Hook _ _) -> not isPlayer
		     (Block _) -> null springs
		     _ -> False
        mismatches = [ s | (s,force') <- sforces, idx == forceIdx force', force /= force' ]
	(pieceCollisions, collisionPoints) = unzip [ (s,pos) |
	    (s,force') <- sforces
	    , let idx' = forceIdx force'
	    , idx /= idx'
	    , not $ forceIsNull force'
	    , let st' = applyForce force $ applyForce force' st
	    , pos <- collisions st' idx idx' ]
	blocks = mismatches ++ nub pieceCollisions
	selfBlocked = or [fixed
	    , not $ null blockingSprings
	    , not $ null linkCollisions ]
    in do
	if selfBlocked
	    then return $ Unmoving
	    else if null blocks
		then return $ Moving []
		else do when isPlayer $ tell $ map AlertCollision collisionPoints
			return $ Moving $ source:blocks

immediatelyBlocked :: GameState -> Force -> Bool
immediatelyBlocked st@(GameState pps conns) f@(Push idx dir) =
    let pp@(PlacedPiece pos piece) = getpp st idx
	springs = springsEndAtIdx st idx
        blockingSprings = [ c |
	    c@(Connection _ _ (Spring sdir _)) <- springsAtIdx st idx
	    , not $ dir `elem` [sdir, neg sdir] ]
	fixed = case piece of
		     (Block _) -> or [null springs, not $ null blockingSprings]
		     (Pivot _) -> isPush f
		     (Wrench _) -> True
		     (Hook _ _) -> True
		     _ -> False
    in fixed
immediatelyBlocked _ (Torque _ _) = False


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
	    Hook _ False -> armPush
	    _ -> (Push idx dir)
propagateForce :: GameState -> Force -> [Force]
propagateForce st@(GameState plPieces conns) force = 
    bumps ++ springTransfers
    where
	bumps = [ transferredForce st idx' cpos dir |
	    let pp = applyForceTo (getForcedpp st force) force
	    , (idx',pp') <- enumVec plPieces
	    , idx' /= forceIdx force
	    , cpos <- collidePieces pp pp'
	    , let dir = case force of
		    Push _ dir -> dir
		    Torque _ dir -> if immediatelyBlocked st (transferredForce st idx' cpos push)
					 then claw else push
			where push = arm <-> rotate (-dir) arm
			      claw = rotate dir arm <-> arm
			      arm = cpos <-> placedPos pp ]
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
