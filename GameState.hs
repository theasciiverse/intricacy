module GameState where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Data.Maybe
import Control.Monad
import Data.List
import Data.Vector (Vector, (!), (!?), (//))

import Hex

type HexPos = PHS HexVec

data GameState = GameState { placedPieces :: Vector PlacedPiece,
	connections :: [Connection] }
    deriving (Eq, Ord, Show)

data PlayState = PlayState {psGameState::GameState, wrenchSelected::Bool }
    deriving (Eq, Ord, Show)
data EditState = EditState {esGameState::GameState
	, selectedPiece::Maybe PieceIdx, selectedPos::HexPos}
    deriving (Eq, Ord, Show)

data PlacedPiece = PlacedPiece { placedPos :: HexPos, placedPiece :: Piece }
    deriving (Eq, Ord, Show)

getpp :: GameState -> PieceIdx -> PlacedPiece
getpp st idx = fromJust $ (placedPieces st) !? idx

addpp :: PlacedPiece -> GameState -> GameState
addpp pp st@(GameState pps _) = st {placedPieces = Vector.snoc pps pp}

addConn :: Connection -> GameState -> GameState
addConn conn st@(GameState _ conns) = st {connections = conn:conns}

addBlockPos :: PieceIdx -> HexPos -> GameState -> GameState
addBlockPos b pos st =
    let PlacedPiece ppos (Block patt) = getpp st b
    in setPiece b (Block (pos <-> ppos:patt)) st

addPivotArm :: PieceIdx -> HexPos -> GameState -> GameState
addPivotArm p pos st =
    let PlacedPiece ppos (Pivot arms) = getpp st p
    in setPiece p (Pivot (pos <-> ppos:arms)) st

delPiecePos :: PieceIdx -> HexPos -> GameState -> GameState
delPiecePos idx pos st =
    let PlacedPiece ppos p = getpp st idx
	rpos = pos <-> ppos
    in (case p of
	   Block patt -> if patt == [rpos]
	       then delPiece idx
	       else setPiece idx $ Block $ patt \\ [rpos]
	   Pivot arms -> if rpos == zero
	       then delPiece idx
	       else setPiece idx $ Pivot $ arms \\ [rpos]
	   _ -> delPiece idx
       ) st

delConnectionAt :: HexPos -> GameState -> GameState
delConnectionAt pos st =
    st {connections = filter
	(\conn -> not $ pos `elem` (connectionFootPrint st conn))
        $ connections st}

delPiece :: PieceIdx -> GameState -> GameState
delPiece idx st@(GameState pps conns) =
    GameState (Vector.ifilter (\idx' -> \_ -> idx' /= idx) pps)
	(filter (\(Connection (ridx,_) (eidx,_) _) ->
	    ridx /= idx && eidx /= idx) conns)

enumVec :: Vector a -> [(Int,a)]
enumVec = Vector.toList . Vector.indexed

setPiece :: PieceIdx -> Piece -> GameState -> GameState
setPiece idx p st@(GameState pps _) = 
    st { placedPieces = pps //
	[(idx, PlacedPiece (placedPos $ getpp st idx) p)] }

data Piece = Block { blockPattern :: [HexVec] }
	    | Pivot { pivotArms :: [HexDir] }
	    | Hook { hookArm :: HexDir, hookTorqueStiff :: Bool}
	    | Wrench { wrenchMomentum :: HexDir }
	    | Ball { ballMomentum :: HexDir }
    deriving (Eq, Ord, Show)
isBlock p = case p of Block _ -> True; _ -> False
isPivot p = case p of Pivot _ -> True; _ -> False
isHook p = case p of Hook _ _ -> True; _ -> False
isPlayerPiece p = isPivot p || isHook p
isWrench p = case p of Wrench _ -> True; _ -> False
isBall p = case p of Ball _ -> True; _ -> False

type PieceIdx = Int

type Locus = (PieceIdx, HexVec)
locusPos :: GameState -> Locus -> HexPos
locusPos s (idx,v) = v <+> (placedPos $ getpp s idx)

data Connection = Connection { connectionRoot :: Locus
		    , connectionEnd :: Locus, link :: Link }
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
    [ c | c@(Connection (ridx,_) (eidx, _) (Spring _ _)) <- connections st
	  , idx `elem` [ridx,eidx] ]

springsEndAtIdx st idx =
    [ c | c@(Connection _ (eidx, _) (Spring _ _)) <- connections st
	  , eidx==idx ]

data SpringExtension = Relaxed | Compressed | Stretched
    deriving (Eq, Ord, Show)

data Tile = BlockTile | PivotTile HexDir | ArmTile HexDir Bool | HookTile | WrenchTile Bool
	  | BallTile Bool | SpringTile SpringExtension
    deriving (Eq, Ord, Show)

type OwnedTile = (PieceIdx, Tile)
type GameBoard = Map HexPos OwnedTile

data Alert = AlertCollision HexPos

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
    let overarmed = length arms > 2 in
    Map.fromList $ (pos, PivotTile $ if overarmed then (head arms) else zero ) : 
	[ (rel <+> pos, ArmTile rel main) |
	    (rel,main) <- zip arms $ repeat False -- overarmed:$repeat False
	    ]
plPieceMap (PlacedPiece pos (Hook arm _)) =
    Map.fromList $ (pos, HookTile) : [ (arm <+> pos, ArmTile arm True) ]
plPieceMap (PlacedPiece pos (Wrench mom)) = Map.singleton pos $ WrenchTile (mom /= zero)
plPieceMap (PlacedPiece pos (Ball mom)) = Map.singleton pos $ BallTile (mom /= zero)

plPieceFootprint :: PlacedPiece -> [HexPos]
plPieceFootprint = Map.keys . plPieceMap

fullFootprint :: GameState -> PieceIdx -> [HexPos]
fullFootprint st idx = plPieceFootprint (getpp st idx) ++
    (concat $ map (connectionFootPrint st) $ springsAtIdx st idx)

-- TODO: use bounding boxes to optimise collision detection?
collidePieces :: PlacedPiece -> PlacedPiece -> [HexPos]
collidePieces pp pp' =
    intersect (plPieceFootprint pp) (plPieceFootprint pp')
piecesIntersect :: PlacedPiece -> PlacedPiece -> Bool
piecesIntersect pp pp' = not $ null $ collidePieces pp pp'

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

castRay :: HexPos -> HexDir -> GameBoard -> Maybe (PieceIdx, HexPos)
castRay start dir board =
    castRay' 30 start
    where castRay' 0 _ = Nothing
	  castRay' n pos =
	      case Map.lookup pos board of
		  Nothing -> castRay' (n-1) (dir<+>pos)
		  Just (idx,_) -> Just (idx,pos)


type PieceColouring = Map PieceIdx Int
type Graph a = (Set a, Set (Set a))

colouredPieces :: Bool -> GameState -> [PieceIdx]
colouredPieces colourFixed st = [ idx |
    (idx, PlacedPiece _ p) <- enumVec $ placedPieces st
    , isPivot p ||
	and [ isBlock p, idx > 0
	      , or [ colourFixed, not $ null $ springsEndAtIdx st idx ] ] ]

-- TODO: try previous colouring first, and pref aim to make minimal changes
-- to it if it's no longer valid
boardColouring :: GameBoard -> [PieceIdx] -> PieceColouring -> PieceColouring
boardColouring board coloured last = fourColour adjGraph last
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

fourColour :: Ord a => Graph a -> Map a Int -> Map a Int
fourColour (nodes,edges) last =
    -- Just bruteforce it. TODO: implement Wernicke's five-colouring
    -- trick - see https://en.wikipedia.org/wiki/Five_color_theorem 
    if Map.size last == Set.size nodes && isColouring last
	then last
	else head $ filter isColouring colourings
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
	
modTile :: Maybe Tile -> HexPos -> GameState -> GameState
modTile tile pos st =
    let board = stateBoard st
	curOwnedTile = Map.lookup pos board
	st' = case curOwnedTile of
	    Nothing -> st
	    Just (owner,SpringTile _) -> delConnectionAt pos st
	    Just (owner,_) -> delPiecePos owner pos st
	addPiece p = addpp $ PlacedPiece pos p
    in (case tile of
	   Nothing -> id
	   Just BlockTile ->
	       let adjacentBlocks = [ idx |
		       dir <- hexDirs
		       , Just (idx, BlockTile) <- [Map.lookup (dir <+> pos) board] ]
		   adjacentBlocks' = case curOwnedTile of
			Just (owner,BlockTile) -> case elemIndex owner adjacentBlocks of
				Nothing -> adjacentBlocks
				Just i -> dropWhile (== owner) $ drop i adjacentBlocks
			_ -> adjacentBlocks
	       in case adjacentBlocks' of
		      [] -> addPiece $ Block [zero]
		      b:bs -> addBlockPos b pos
	   Just (ArmTile armdir _) -> 
	       let adjacentPivots = [ idx |
		       dir <- [armdir, neg armdir]
		       , Just (idx, PivotTile _) <- [Map.lookup (dir <+> pos) board] ]
		   adjacentPivots' = case curOwnedTile of
			Just (owner,ArmTile armdir' _) | armdir' `elem` [armdir, neg armdir] ->
			    dropWhile (== owner) adjacentPivots
			_ -> adjacentPivots
	       in case adjacentPivots' of
		      [] -> id
		      p:_ -> addPivotArm p pos
	   Just (SpringTile extn) ->
	       let possibleSprings = [ Connection root end $ Spring sdir natLen |
		       sdir <- hexDirs
		       , let epos = sdir <+> pos
		       , Just (eidx, BlockTile) <- [Map.lookup epos board]
		       , (ridx, rpos) <- maybeToList $ castRay (neg sdir <+> pos) (neg sdir) board
		       , Block _ <- [ placedPiece $ getpp st ridx]
		       , let natLen = hexLen (rpos <-> epos) - 1
		       , natLen > 1
		       , let end = (eidx, epos <-> (placedPos $ getpp st eidx))
		       , let root = (ridx, rpos <-> (placedPos $ getpp st ridx))
		       ]
		   possibleSprings' =
			case findIndex (`elem` connections st) possibleSprings of
			    Nothing -> possibleSprings
			    Just i -> drop i possibleSprings
	       in case possibleSprings' of
		      [] -> id
		      conn:_ -> addConn conn
	   Just (PivotTile _) -> addPiece $ Pivot []
	   Just (WrenchTile _) -> addPiece $ Wrench zero
	   Just HookTile -> let arm = listToMaybe [ dir | 
				    dir <- hexDirs
				    , Map.lookup (dir <+> pos) board == Nothing ]
			    in case arm of Just armdir -> addPiece $ Hook armdir False
					   _ -> id
	   Just (BallTile _) -> addPiece $ Ball zero
	) st'
