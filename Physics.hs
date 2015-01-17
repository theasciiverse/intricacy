-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Physics where

import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Control.Monad.State
import Control.Monad.Writer
import Data.Foldable (foldrM)
import Data.List
import Data.Vector (Vector, (!), (//))

import Hex
import Util
import GameState
import GameStateTypes

-- | a list of forces to try in order:
newtype ForceChoice = ForceChoice {getForceChoice :: [Force]}

type ForceChoices = Vector.Vector ForceChoice

forceIdx :: Force -> PieceIdx
forceIdx force = case force of (Push idx _) -> idx
			       (Torque idx _) -> idx

isPush,isTorque,forceIsNull :: Force -> Bool
isPush (Push _ _) = True
isPush _ = False
isTorque = not . isPush
forceIsNull (Push _ dir) = dir == zero
forceIsNull (Torque _ dir) = dir == 0

getForcedpp :: GameState -> Force -> PlacedPiece
getForcedpp s f = getpp s (forceIdx f)

-- |PlayerMove: if not NullPM, the direction should be non-zero
data PlayerMove = NullPM | HookPush HexDir | HookTorque TorqueDir | WrenchPush HexDir
    deriving (Eq, Ord, Show, Read)

toolForces :: GameState -> PlayerMove -> ForceChoices
toolForces st pm = Vector.fromList $
    [ ForceChoice (wmom++wmove)
    | (widx, PlacedPiece _ (Wrench mom)) <- epps
    , let wmom = if mom == zero then [] else [Push widx mom]
    , let wmove = case pm of {WrenchPush v -> [Push widx v]; _ -> []}
    , not $ null (wmom++wmove)
    ] ++ case pm of
	HookTorque ht -> [ ForceChoice [Torque hidx ht] | hidx <- hidxs ]
	HookPush hp -> [ ForceChoice [Push hidx hp] | hidx <- hidxs ]
	_ -> []
    where
	epps = enumVec $ placedPieces st
	hidxs = [ hidx | (hidx, PlacedPiece _ (Hook _ _)) <- epps ]

-- |Dominance: a propagand of a source force F can not block any progagand of
-- a source force which dominates F.
-- F dominates F' iff F and F' are spring forces, and the end of F is an
-- ancestor of the root of F'.
-- Note that by acyclicity of the connection digraph, domination is
-- antisymmetric.
type Dominance = Int -> Int -> Bool

envForces :: GameState -> (ForceChoices, Dominance)
envForces st@(GameState _ conns) =
    let rootedForces :: Vector (Maybe PieceIdx, Force)
	rootedForces = Vector.fromList [ (Just rootIdx, Push endIdx dir)
	    | c@(Connection (rootIdx,_) (endIdx,_) (Spring outDir natLen)) <- conns
	    , let curLen = connectionLength st c
	    , natLen /= curLen
	    , let dir = if natLen > curLen then outDir else neg outDir ]

    in ( Vector.map (ForceChoice . replicate 1 . snd) rootedForces, 
	\f1 f2 -> Just True == do
		rootIdx <- fst $ rootedForces!f2
		return $ connGraphPathExists st (forceIdx.snd $ rootedForces!f1) rootIdx )
	

setTools :: PlayerMove -> GameState -> GameState
setTools pm st =
    let hf = case pm of
	    HookTorque dir -> TorqueHF dir
	    HookPush v -> PushHF v
	    _ -> NullHF
    in adjustPieces (\p -> case p of
		Hook arm _ -> Hook arm hf
		_ -> p) st

physicsTick :: PlayerMove -> GameState -> Writer [Alert] GameState
physicsTick pm st =
    let tfs = toolForces st pm
	(efs, dominates) = envForces st
    in do
	st' <- resolveForces tfs Vector.empty (\_ _->False) $ setTools pm st
	resolveForces Vector.empty efs dominates $ setTools NullPM st'

type Source = Int
data SourcedForce = SForce Source Force Bool Bool
    deriving (Eq, Ord, Show)
resolveForces :: ForceChoices -> ForceChoices -> Dominance -> GameState -> Writer [Alert] GameState
resolveForces plForces eForces eDominates st =
    let pln = Vector.length plForces
	dominates i j = case map (< pln) [i,j] of
		[True,False] -> True
		[False,False] -> eDominates (i-pln) (j-pln)
		_ -> False
	initGrps = fmap (propagate st True) plForces Vector.++
		fmap (propagate st False) eForces 
	blockInconsistent :: Int -> Int -> StateT (Vector (Writer Any [Force])) (Writer [Alert]) ()
	blockInconsistent i j = do
	    grps <- mapM gets [(!i),(!j)]
	    blocks <- lift $ checkInconsistent i j $ map (fst.runWriter) grps
	    modify $ Vector.imap (\k -> if k `elem` blocks then (tell (Any True) >>) else id)
	checkInconsistent :: Int -> Int -> [[Force]] -> Writer [Alert] [Int]
	checkInconsistent i j fss = 
	    let st' = foldr applyForce st $ nub $ concat fss
		inconsistencies = [ [f,f']
		    | [f,f'] <- sequence fss
		    , if forceIdx f == forceIdx f'
			then f /= f'
			else not.null $ collisions st' (forceIdx f) (forceIdx f') ]
	    in do
		tell $ map AlertBlockingForce $ concat inconsistencies
		return $ if null inconsistencies then []
		    else if i==j then [i]
		    else if dominates i j then [j]
		    else if dominates j i then [i]
		    else [i,j]

	stopWrench idx = setPiece idx (Wrench zero)
	stopBlockedWrenches fs st' = foldr stopWrench st' [
	    forceIdx f | f <- fs, isWrench.placedPiece $ getForcedpp st' f ]
	divertedWrenches fs = [ idx
	    | Push idx dir <- fs
	    , Wrench mom <- [placedPiece $ getpp st idx]
	    , mom `notElem` [zero,dir] ]
    in do 
	let unresisted = [ s | (s, (_, Any False)) <- enumVec $ fmap runWriter initGrps ]

	-- check for inconsistencies within, and between pairs of, forcegroups
	grps <- sequence [ blockInconsistent i j
			| [i,j] <- sequence [unresisted,unresisted]
			, i <= j ]
		    `execStateT` initGrps

	let [blocked, unblocked] = map (nub.concat.(map (fst.runWriter)).Vector.toList) $
		(\(x,y) -> [x,y]) $ Vector.partition (getAny.snd.runWriter) grps
	tell $ map AlertBlockedForce blocked
	tell $ map AlertAppliedForce unblocked
	tell $ map AlertDivertedWrench $ divertedWrenches unblocked
	return $ stopBlockedWrenches blocked $ foldr applyForce st unblocked
    
applyForce :: Force -> GameState -> GameState
applyForce f s =
    let idx = forceIdx f
	pp' = applyForceTo (getpp s idx) f
	pp'' = case (placedPiece pp',f) of
		   ( Wrench _ , Push _ dir ) -> pp' {placedPiece = Wrench dir}
		   _ -> pp'
	in 
	s { placedPieces = (placedPieces s) // [(idx, pp'')] }

collisionsWithForce :: GameState -> Force -> PieceIdx -> [HexPos]
collisionsWithForce st (Push idx dir) idx' =
    intersect (map (dir<+>) $ footprintAtIgnoring st idx idx') (footprintAtIgnoring st idx' idx)
collisionsWithForce st force idx' =
    collisions (applyForce force st) (forceIdx force) idx'

applyForceTo :: PlacedPiece -> Force -> PlacedPiece
applyForceTo (PlacedPiece pos piece) (Push _ dir) =
    PlacedPiece (dir <+> pos) piece
applyForceTo (PlacedPiece pos (Pivot arms)) (Torque _ dir) =
    PlacedPiece pos (Pivot $ map (rotate dir) arms)
applyForceTo (PlacedPiece pos (Hook arm hf)) (Torque _ dir) =
    PlacedPiece pos (Hook (rotate dir arm) hf)
applyForceTo pp _ = pp

-- A force on a piece which resists it is immediately blocked
pieceResists :: GameState -> Force -> Bool 
pieceResists st force =
    let idx = forceIdx force
	PlacedPiece _ piece = getpp st idx
        springs = springsEndAtIdx st idx
	fixed = case piece of
		     (Pivot _) -> isPush force
		     (Block _) -> null springs
		     (Wrench mom) -> case force of
			    Push _ v -> v /= mom
			    _ -> True
		     (Hook _ hf) -> case force of 
			    Push _ v -> hf /= PushHF v
			    Torque _ dir -> hf /= TorqueHF dir
		     _ -> False
    in fixed

-- |transmittedForce: convert pushes into torques as appropriate
transmittedForce :: GameState -> Source -> HexPos -> HexDir -> Force
transmittedForce st idx cpos dir =
    let pp@(PlacedPiece _ piece) = getpp st idx
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
propagateForce :: GameState -> Bool -> Force -> [ForceChoice]
propagateForce st@(GameState _ conns) isPlSource force =
    bumps ++ springTransmissions
    where
	idx = forceIdx force
	bumps = [ ForceChoice $ map (transmittedForce st idx' cpos) dirs |
	    idx' <- ppidxs st
	    , idx' /= idx
	    , cpos <- collisionsWithForce st force idx'
	    , let dirs = case force of
		    Push _ dir -> [dir]
		    Torque _ dir -> [push,claw]
			where push = arm <-> rotate (-dir) arm
			      claw = rotate dir arm <-> arm
			      arm = cpos <-> placedPos (getpp st idx) ]
	springTransmissions =
	    case force of
		 Push _ dir -> [ ForceChoice [Push idx' dir] |
			c@(Connection (ridx,_) (eidx,_) (Spring sdir _)) <- conns
			, let root = idx == ridx
			, let end = idx == eidx
			, root || end
			, let idx' = if root then eidx else ridx
			, let pull = (root && dir == neg sdir) || (end && dir == sdir)
			, let push = (root && dir == sdir) || (end && dir == neg sdir)
			, (push && if isPlSource then springFullyCompressed st c else not $ springExtended st c) ||
			    (pull && if isPlSource then springFullyExtended st c else not $ springCompressed st c) ||
			    (not push && not pull) ]
		 _ -> []

-- |propagate: find forcegroup generated by a forcechoice, and note if the
-- group is blocked due to resistance. If there are multiple forces in a
-- forcechoice and the first results in a block due to resistance, try the
-- next instead.
propagate :: GameState -> Bool -> ForceChoice -> Writer Any [Force]
propagate st isPlSource fch = Set.toList `liftM` propagate' isPlSource Set.empty fch where
    propagate' isPlForce ps (ForceChoice (f:backups)) =
	if f `Set.member` ps then return ps
	else
	    let (ps', failed) = if pieceResists st f && (not isPlForce)
		    then (ps, Any True)
		    else runWriter $ foldrM
			    (flip $ propagate' False)
			    (f `Set.insert` ps)
			    $ propagateForce st isPlSource f
	    in if getAny failed
		then if null backups
		    then tell (Any True) >> return ps'
		    else propagate' isPlForce ps $ ForceChoice backups
		else return ps'
    propagate' _ _ (ForceChoice []) = error "null ForceChoice"
