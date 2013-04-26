module GameState where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Vector (Vector, (!), (!?), (//))

import Hex
import Debug

type HexPos = PHS HexVec
origin :: HexPos
origin = PHS zero

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

ppidxs :: GameState -> [PieceIdx]
ppidxs = Vector.toList . (Vector.findIndices $ \_ -> True) . placedPieces

getpp :: GameState -> PieceIdx -> PlacedPiece
getpp st idx = (placedPieces st) ! idx

setpp :: PieceIdx -> PlacedPiece -> GameState -> GameState
setpp idx pp st@(GameState pps _) = st {placedPieces = pps // [(idx, pp)] }

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

delPiecePos :: PieceIdx -> HexPos -> GameState -> (GameState, Maybe PieceIdx)
-- ^ returns new state and the new index of what remains of the piece, if
-- anything
delPiecePos idx pos st =
    let PlacedPiece ppos p = getpp st idx
	v = pos <-> ppos
    in case p of
        Block patt ->
	    let comps = components $ Set.fromList $ patt \\ [v]
		ppOfComp (v,patt) = PlacedPiece (v<+>ppos) $ Block $ Set.toList patt
		(st',midx) = case comps of
		    [] -> (delPiece idx st, Nothing)
		    zeroComp:newComps ->
			(setpp idx (ppOfComp zeroComp)
			    $ foldr (addpp . ppOfComp) st newComps, Just idx)
	    in (repossessConns st st', midx)
        Pivot arms -> if v == zero
	   then (delPiece idx st, Nothing)
	   else ((setPiece idx $ Pivot $ arms \\ [v]) st, Just idx)
        _ -> (delPiece idx st, Nothing)

type Component = (HexVec, Set HexVec)
components :: Set HexVec -> [Component]
components patt
    | Set.null patt = []
    | otherwise =
	let c = if zero `Set.member` patt then zero else head $ Set.toList patt
	    (patt',comp) = floodfill c patt
	in ( (c, Set.map (<+> neg c) comp) : components patt' )

floodfill :: HexVec -> Set HexVec -> (Set HexVec, Set HexVec)
floodfill start patt = floodfill' start `execState` (patt, Set.empty)
    where 
	floodfill' :: HexVec -> State (Set HexVec, Set HexVec) ()
	floodfill' start = do
	      (patt, dels) <- get
	      let patt' = Set.delete start patt
	      if Set.size patt' == Set.size patt
		  then return ()
		  else do
		      put (patt', Set.insert start dels)
		      sequence_ [ floodfill' (dir<+>start) | dir <- hexDirs ]

repossessConns :: GameState -> GameState -> GameState
repossessConns st st' =
    st' {connections = [ Connection root' end' link |
	Connection root end link <- connections st
	, root' <- maybeToList $ posLocus st' $ locusPos st root
	, end' <- maybeToList $ posLocus st' $ locusPos st end ] }

delConnectionsIn :: HexPos -> GameState -> GameState
delConnectionsIn pos st =
    st {connections = filter
	(\conn -> not $ pos `elem` (connectionFootPrint st conn))
        $ connections st}

delConnectionsAt :: Locus -> GameState -> GameState
delConnectionsAt locus st =
    st {connections = filter
	(\conn -> not $ locus `elem` [connectionRoot conn, connectionEnd conn])
        $ connections st}

delPiece :: PieceIdx -> GameState -> GameState
delPiece idx st@(GameState pps conns) =
    GameState (Vector.concat [Vector.take idx pps, Vector.drop (idx+1) pps])
	[ Connection (ridx',rv) (eidx',ev) link |
	    Connection (ridx,rv) (eidx,ev) link <- conns
	    , ridx /= idx
	    , eidx /= idx
	    , let ridx' = if ridx > idx then ridx-1 else ridx
	    , let eidx' = if eidx > idx then eidx-1 else eidx ]

enumVec :: Vector a -> [(Int,a)]
enumVec = Vector.toList . Vector.indexed

setPiece :: PieceIdx -> Piece -> GameState -> GameState
setPiece idx p st@(GameState pps _) = 
    setpp idx (PlacedPiece (placedPos $ getpp st idx) p) st

adjustPieces :: (Piece -> Piece) -> GameState -> GameState
adjustPieces f st =
    st { placedPieces = fmap
	(\pp -> pp { placedPiece = f $ placedPiece pp })
       $ placedPieces st }

data Stiffness = Unstiff | PushStiff | TorqueStiff | ForceStiff
    deriving (Eq, Ord, Show)

data Piece = Block { blockPattern :: [HexVec] }
	    | Pivot { pivotArms :: [HexDir] }
	    | Hook { hookArm :: HexDir, hookStiffness :: Stiffness}
	    | Wrench { wrenchMomentum :: HexDir }
	    | Ball { ballMomentum :: HexDir }
    deriving (Eq, Ord, Show)
isBlock p = case p of Block _ -> True; _ -> False
isPivot p = case p of Pivot _ -> True; _ -> False
isHook p = case p of Hook _ _ -> True; _ -> False
isWrench p = case p of Wrench _ -> True; _ -> False
isTool p = isWrench p || isHook p
isBall p = case p of Ball _ -> True; _ -> False

type PieceIdx = Int

type Locus = (PieceIdx, HexVec)
locusPos :: GameState -> Locus -> HexPos
locusPos s (idx,v) = v <+> (placedPos $ getpp s idx)

posLocus :: GameState -> HexPos -> Maybe Locus
posLocus st pos = listToMaybe [ (idx,pos<->ppos) |
    (idx,pp@(PlacedPiece ppos _)) <- enumVec $ placedPieces st
    , pos `elem` plPieceFootprint pp ]

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

springsRootAtIdx st idx =
    [ c | c@(Connection (ridx, _) _ (Spring _ _)) <- connections st
	  , ridx==idx ]

connGraphPathExists :: GameState -> PieceIdx -> PieceIdx -> Bool
connGraphPathExists st ridx eidx =
    or [ridx == eidx 
	, or $ map (connGraphPathExists st eidx)
	    $ map (fst . connectionEnd) (springsRootAtIdx st ridx) ]

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
plPieceBoard (idx,pp) = fmap (\x -> (idx,x)) $ plPieceMap pp

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

collisions :: GameState -> PieceIdx -> PieceIdx -> [HexPos]
-- ^intersections of two pieces and their connections, disregarding
-- the connections which connect the two pieces
collisions st idx idx' =
    intersect (fullFootprint st idx) (fullFootprint st idx') \\
	(concat $ map (connectionFootPrint st)
	    $ filter (\conn ->
		Set.fromList [fst $ connectionRoot conn, fst $ connectionEnd conn] ==
		    Set.fromList [idx,idx'])
	    $ connections st)

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

colouredPieces :: Bool -> GameState -> [PieceIdx]
colouredPieces colourFixed st = [ idx |
    (idx, PlacedPiece _ p) <- enumVec $ placedPieces st
    , isPivot p ||
	and [ isBlock p, idx > 0
	      , or [ colourFixed, not $ null $ springsEndAtIdx st idx ] ] ]

type Graph a = (Set a, Set (Set a))
type PlanarGraph a = Map a [a]

boardColouring :: GameState -> [PieceIdx] -> PieceColouring -> PieceColouring
boardColouring st coloured last =
   fiveColour graph last
    where
	board = stateBoard st
	touchings :: Map PieceIdx (Map PieceIdx HexVec)
	touchings = foldr addPoint Map.empty $ Map.toList board
	addPoint (pos, (owner,_)) ts
	    | not $ owner `elem` coloured = ts
	    | otherwise =
		let ppos = placedPos $ getpp st owner
		    adjs = Map.findWithDefault Map.empty owner ts
		    adjs' = foldr (uncurry Map.insert) adjs
			[ (owner', pos' <-> ppos) |
			    pos' <- map (\d -> d<+>pos) hexDirs
			    , Just (owner',_) <- [Map.lookup pos' board]
			    , owner' /= owner
			    , owner' `elem` coloured ]
		in Map.insert owner adjs' ts
	graph =
	    fmap (\adjs ->
		sortBy (\a a' -> cmpAngles (adjs Map.! a) (adjs Map.! a'))
		    $ Map.keys adjs) touchings

type Colouring a = Map a Int

fourColour :: Ord a => Graph a -> Colouring a -> Colouring a
fourColour (nodes,edges) last =
    -- bruteforce 
    if Map.keysSet last == nodes && isColouring last
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

fiveColour :: Ord a => PlanarGraph a -> Colouring a -> Colouring a
-- ^algorithm based on that presented in
-- http://people.math.gatech.edu/~thomas/PAP/fcstoc.pdf 
-- Key point: a planar graph can't have all vertices of degree >= 6
-- (Proof: suppose it does, so |E| >= 3|V|; WLOG the graph is triangulated,
-- so then |F| <= 2/3 |E|. So \xi = |V|-|E|+|F| <= (1/3 - 1 + 2/3)|E| = 0.
-- But a planar graph has Euler characteristic 1.)
fiveColour g last =
    if Map.keysSet last == Map.keysSet g && isColouring last
	then last
	else fiveColour' g
    where
    isColouring mapping = and [
	Map.lookup s mapping /= Map.lookup e mapping |
	    s <- Map.keys g
	    , e <- g Map.! s ]
    fiveColour' :: Ord a => PlanarGraph a -> Colouring a
    fiveColour' g
	| g == Map.empty = Map.empty
	| otherwise =
	let v = head $ filter (\v -> length (nub $ g Map.! v) <= 5) $ Map.keys g
	    adjs = nub $ g Map.! v
	    addTo c = let vc = head $ [0..4] \\ map (c Map.!) adjs
		      in Map.insert v vc c
	in if length adjs < 5
	   then addTo $ fiveColour' $ deleteNode v g
	   else let (v',v'') = if adjs!!2 `elem` (g Map.! (adjs!!0))
			then (adjs!!1,adjs!!3)
			else (adjs!!0,adjs!!2)
		in addTo $ demerge v' v'' $ fiveColour' $ merge v v' v'' g
    demerge :: Ord a => a -> a -> Colouring a -> Colouring a
    demerge v v' c = Map.insert v' (c Map.! v) c
    merge :: Ord a => a -> a -> a -> PlanarGraph a -> PlanarGraph a
    merge v v' v'' g =
	deleteNode v $ contractNodes v' v''
	    $ Map.adjust (concatAdjsOver v $ g Map.! v'') v' g
    concatAdjsOver :: Ord a => a -> [a] -> [a] -> [a]
    concatAdjsOver v adjs adjs' =
	let (s,_:e) = splitAt (fromJust $ elemIndex v adjs) adjs
	in s ++ adjs' ++ e
    deleteNode v =
	fmap (filter (/= v)) . Map.delete v
    contractNodes v v' =
	fmap (map (\v'' -> if v'' == v' then v else v'')) . Map.delete v'
    


	
modTile :: Maybe Tile -> HexPos -> GameState -> GameState
modTile tile pos st =
    let board = stateBoard st
	curOwnedTile = Map.lookup pos board
	(st',mowner) = case curOwnedTile of
	    Nothing -> (st,Nothing)
	    Just (owner,SpringTile _) -> (delConnectionsIn pos st, Just owner)
	    Just (owner,_) -> delPiecePos owner pos st -- XXX may invalidate board's indices to st
	board' = stateBoard st'
	addPiece p = addpp $ PlacedPiece pos p
    in case mowner of
	 Just o | protectedPiece o -> st
	 _ -> (case tile of
	   Nothing -> id
	   Just BlockTile ->
	       let adjacentBlocks = [ idx |
		       dir <- hexDirs
		       , Just (idx, BlockTile) <- [Map.lookup (dir <+> pos) board']
		       , not $ protectedPiece idx ]
		   adjacentBlocks' = 
			fromMaybe adjacentBlocks $ do
			    (_,BlockTile) <- curOwnedTile
			    owner <- mowner
			    i <- elemIndex owner adjacentBlocks
			    return $ dropWhile (== owner) $ drop i adjacentBlocks
	       in case adjacentBlocks' of
		      [] -> addPiece $ Block [zero]
		      b:bs -> addBlockPos b pos
	   Just (ArmTile armdir _) -> 
	       let adjacentPivots = [ idx |
		       dir <- [armdir, neg armdir]
		       , Just (idx, PivotTile _) <- [Map.lookup (dir <+> pos) board'] ]
		   adjacentPivots' = 
			fromMaybe adjacentPivots $ do
			    (_,ArmTile armdir' _) <- curOwnedTile
			    guard $ armdir' `elem` [armdir, neg armdir] 
			    owner <- mowner
			    return $ dropWhile (== owner) adjacentPivots
	       in case adjacentPivots' of
		      [] -> id
		      p:_ -> addPivotArm p pos
	   Just (SpringTile extn) ->
	       let possibleSprings = [ Connection root end $ Spring sdir natLen |
		       sdir <- hexDirs
		       , let epos = sdir <+> pos
		       , Just (eidx, BlockTile) <- [Map.lookup epos board']
		       , not $ protectedPiece eidx
		       , (ridx, rpos) <- maybeToList $ castRay (neg sdir <+> pos) (neg sdir) board'
		       , Just (_, BlockTile) <- [Map.lookup rpos board']
		       , let natLen = hexLen (rpos <-> epos) - 1
		       , natLen > 1
		       , null [ conn |
			   conn@(Connection _ _ (Spring sdir' _)) <-
			       springsAtIdx st' eidx ++ springsEndAtIdx st' ridx
			   , not $ sdir' `elem` [sdir,neg sdir] ]
		       , not $ connGraphPathExists st' eidx ridx
		       , let end = (eidx, epos <-> (placedPos $ getpp st eidx))
		       , let root = (ridx, rpos <-> (placedPos $ getpp st ridx))
		       ]
		   possibleSprings' = fromMaybe possibleSprings $ do
		       (_,SpringTile _) <- curOwnedTile -- XXX: therefore the indices of st are still valid
		       i <- findIndex (`elem` connections st) possibleSprings
		       return $ drop (i+1) possibleSprings
	       in case possibleSprings' of
		      [] -> id
		      conn:_ -> addConn conn
	   Just (PivotTile _) -> addPiece $ Pivot []
	   Just (WrenchTile _) -> addPiece $ Wrench zero
	   Just HookTile -> let arm = listToMaybe [ dir | 
				    dir <- hexDirs
				    , Map.lookup (dir <+> pos) board' == Nothing ]
			    in case arm of Just armdir -> addPiece $ Hook armdir Unstiff
					   _ -> id
	   Just (BallTile _) -> addPiece $ Ball zero
	   ) st'

mergeTiles :: HexPos -> HexDir -> Bool -> GameState -> GameState
mergeTiles pos dir mergePiece st = fromMaybe st $ do
    let board = stateBoard st
    (idx,tile) <- Map.lookup pos board
    (idx',tile') <- Map.lookup (dir<+>pos) board
    guard $ idx /= idx'
    guard $ and $ map (not . protectedPiece) [idx,idx']
    case tile of
	BlockTile -> do
	    BlockTile <- Just tile'
	    let st' = if mergePiece
			  then delPiece idx st
			  else fst $ delPiecePos idx pos st
	    (idx'',_) <- Map.lookup (dir<+>pos) $ stateBoard st'
	    return $ if mergePiece
			 then foldr (addBlockPos idx'') st'
				$ plPieceFootprint $ getpp st idx
			 else addBlockPos idx'' pos st'
	ArmTile _ _  -> do
	    PivotTile _ <- Just tile'
	    let st' = fst $ delPiecePos idx pos st
	    (idx'',_) <- Map.lookup (dir<+>pos) $ stateBoard st'
	    return $ addPivotArm idx'' pos st'

protectedPiece :: PieceIdx -> Bool
protectedPiece = isFrame
isFrame :: PieceIdx -> Bool
isFrame = (==0)
