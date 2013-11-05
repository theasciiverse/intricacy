module GameState where

import Control.Applicative hiding ((<*>))
import Data.Function (on)
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Data.List
import Data.Vector (Vector, (!), (//))

import Hex
--import Debug

type HexPos = PHS HexVec
origin :: HexPos
origin = PHS zero

data GameState = GameState { placedPieces :: Vector PlacedPiece,
	connections :: [Connection] }
    deriving (Eq, Ord, Show, Read)

data PlacedPiece = PlacedPiece { placedPos :: HexPos, placedPiece :: Piece }
    deriving (Eq, Ord, Show, Read)

type PieceIdx = Int

ppidxs :: GameState -> [PieceIdx]
ppidxs = Vector.toList . (Vector.findIndices $ const True) . placedPieces

getpp :: GameState -> PieceIdx -> PlacedPiece
getpp st idx = (placedPieces st) ! idx

setpp :: PieceIdx -> PlacedPiece -> GameState -> GameState
setpp idx pp st@(GameState pps _) = st {placedPieces = pps // [(idx, pp)] }

addpp :: PlacedPiece -> GameState -> GameState
addpp pp st@(GameState pps _) = st {placedPieces = Vector.snoc pps pp}

addConn :: Connection -> GameState -> GameState
addConn conn st@(GameState _ conns) = st {connections = conn:conns}

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
	      unless (Set.size patt' == Set.size patt) $ do
		  put (patt', Set.insert start dels)
		  sequence_ [ floodfill' (dir<+>start) | dir <- hexDirs ]

delPiece :: PieceIdx -> GameState -> GameState
delPiece idx (GameState pps conns) =
    GameState (Vector.concat [Vector.take idx pps, Vector.drop (idx+1) pps])
	[ Connection (ridx',rv) (eidx',ev) link |
	    Connection (ridx,rv) (eidx,ev) link <- conns
	    , ridx /= idx
	    , eidx /= idx
	    , let ridx' = if ridx > idx then ridx-1 else ridx
	    , let eidx' = if eidx > idx then eidx-1 else eidx ]

delPieceIn :: HexPos -> GameState -> GameState
delPieceIn pos st =
    case liftM fst $ Map.lookup pos $ stateBoard st of
	Just idx -> delPiece idx st
	_ -> st

enumVec :: Vector a -> [(Int,a)]
enumVec = Vector.toList . Vector.indexed

setPiece :: PieceIdx -> Piece -> GameState -> GameState
setPiece idx p st = 
    setpp idx (PlacedPiece (placedPos $ getpp st idx) p) st

adjustPieces :: (Piece -> Piece) -> GameState -> GameState
adjustPieces f st =
    st { placedPieces = fmap
	(\pp -> pp { placedPiece = f $ placedPiece pp })
       $ placedPieces st }

data HookForce = NullHF | PushHF HexDir | TorqueHF Int
    deriving (Eq, Ord, Show, Read)

data Piece = Block { blockPattern :: [HexVec] }
	    | Pivot { pivotArms :: [HexDir] }
	    | Hook { hookArm :: HexDir, hookForce :: HookForce}
	    | Wrench { wrenchMomentum :: HexDir }
	    | Ball
    deriving (Eq, Ord, Show, Read)
isBlock, isPivot, isHook, isWrench, isTool, isBall :: Piece -> Bool
isBlock p = case p of Block _ -> True; _ -> False
isPivot p = case p of Pivot _ -> True; _ -> False
isHook p = case p of Hook _ _ -> True; _ -> False
isWrench p = case p of Wrench _ -> True; _ -> False
isTool p = isWrench p || isHook p
isBall p = case p of Ball -> True; _ -> False

addBlockPos :: PieceIdx -> HexPos -> GameState -> GameState
addBlockPos b pos st =
    let PlacedPiece ppos (Block patt) = getpp st b
    in setPiece b (Block (pos <-> ppos:patt)) st

addPivotArm :: PieceIdx -> HexPos -> GameState -> GameState
addPivotArm p pos st =
    let PlacedPiece ppos (Pivot arms) = getpp st p
    in setPiece p (Pivot (pos <-> ppos:arms)) st

data Connection = Connection { connectionRoot :: Locus
		    , connectionEnd :: Locus, connectionLink :: Link }
    deriving (Eq, Ord, Show, Read)

type Locus = (PieceIdx, HexVec)
locusPos :: GameState -> Locus -> HexPos
locusPos s (idx,v) = v <+> (placedPos $ getpp s idx)

posLocus :: GameState -> HexPos -> Maybe Locus
posLocus st pos = listToMaybe [ (idx,pos<->ppos) |
    (idx,pp@(PlacedPiece ppos _)) <- enumVec $ placedPieces st
    , pos `elem` plPieceFootprint pp ]

connectionLength :: GameState -> Connection -> Int
connectionLength st (Connection root end _) =
    let rootPos = locusPos st root
	endPos = locusPos st end
    in hexLen (endPos <-> rootPos) - 1

data Link = Free { freePos :: HexVec }
	  | Spring { springDir :: HexDir, springNatLength :: Int }
    deriving (Eq, Ord, Show, Read)

springsAtIdx,springsEndAtIdx,springsRootAtIdx :: GameState -> PieceIdx -> [Connection]
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
connGraphPathExists st ridx eidx = (ridx == eidx) ||
	any ((connGraphPathExists st `flip` eidx) . fst .  connectionEnd)
	    (springsRootAtIdx st ridx)

connGraphHeight :: GameState -> PieceIdx -> Int
connGraphHeight st idx =
    maximum (0 : map ((+1) . connGraphHeight st . fst . connectionRoot) (springsEndAtIdx st idx))

type DiGraph a = Map a (Set a)
checkConnGraphAcyclic :: GameState -> Bool
checkConnGraphAcyclic st =
    let idxs = [0..Vector.length (placedPieces st) - 1]
	leaves dg = map fst $ filter (Set.null . snd) $ Map.toList dg
	checkDiGraphAcyclic dg = case listToMaybe $ leaves dg of
	    Nothing -> Map.null dg
	    Just leaf -> checkDiGraphAcyclic $ Map.delete leaf $ fmap (Set.delete leaf) dg
    in checkDiGraphAcyclic $ Map.fromList 
	[ (idx, Set.fromList $ map (fst.connectionRoot) $ springsEndAtIdx st idx) | idx <- idxs ]

repossessConns :: GameState -> GameState -> GameState
repossessConns st st' =
    st' {connections = [ Connection root' end' link |
	Connection root end link <- connections st
	, root' <- maybeToList $ posLocus st' $ locusPos st root
	, end' <- maybeToList $ posLocus st' $ locusPos st end ] }

delConnectionsIn :: HexPos -> GameState -> GameState
delConnectionsIn pos st =
    st {connections = filter
	((pos `notElem`) . connectionFootPrint st)
        $ connections st}

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

springExtended,springCompressed,springFullyExtended,springFullyCompressed :: GameState -> Connection -> Bool
springExtended st c@(Connection _ _ (Spring _ natLen)) = connectionLength st c > natLen
springExtended _ _ = False
springCompressed st c@(Connection _ _ (Spring _ natLen)) = connectionLength st c < natLen
springCompressed _ _ = False
springFullyExtended st c@(Connection _ _ (Spring _ natLen)) = connectionLength st c >= 2*natLen
springFullyExtended _ _ = False
springFullyCompressed st c@(Connection _ _ (Spring _ natLen)) = connectionLength st c <= (natLen+1)`div`2
springFullyCompressed _ _ = False
springExtensionValid st c@(Connection _ _ (Spring _ natLen)) = let l = connectionLength st c in 
    l >= (natLen+1)`div`2 && l <= 2*natLen
springExtensionValid _ _ = True


data SpringExtension = Relaxed | Compressed | Stretched
    deriving (Eq, Ord, Show, Read)

data Tile = BlockTile [HexDir] | PivotTile HexDir | ArmTile HexDir Bool | HookTile | WrenchTile HexDir
	  | BallTile | SpringTile SpringExtension HexDir
    deriving (Eq, Ord, Show, Read)

type OwnedTile = (PieceIdx, Tile)
type GameBoard = Map HexPos OwnedTile

tileType :: Tile -> Tile
tileType (BlockTile _) = BlockTile []
tileType (PivotTile _) = PivotTile zero
tileType (ArmTile _ _) = ArmTile zero False
tileType (WrenchTile _) = WrenchTile zero
tileType (BallTile) = BallTile
tileType (SpringTile _ _) = SpringTile Relaxed zero
tileType t = t

stateBoard :: GameState -> GameBoard
stateBoard st@(GameState plPieces conns) =
    addConnAdjs st conns $
	(Map.unions $ map plPieceBoard $ enumVec plPieces) `Map.union`
	(Map.unions $ map (connectionBoard st) conns)

addConnAdjs :: GameState -> [Connection] -> GameBoard -> GameBoard
addConnAdjs st = flip $ foldr addConnAdj
    where
	addConnAdj (Connection root end (Spring dir _)) board =
	    addAdj (locusPos st root) dir $
	    addAdj (locusPos st end) (neg dir) board
	addConnAdj _ board = board
	addAdj pos d =
	    Map.adjust (\(o,tile) -> (o,case tile of
		    BlockTile adjs -> BlockTile (d:adjs)
		    _ -> tile))
		pos

plPieceBoard :: (PieceIdx,PlacedPiece) -> GameBoard
plPieceBoard (idx,pp) = fmap (\x -> (idx,x)) $ plPieceMap pp

plPieceMap :: PlacedPiece -> Map HexPos Tile
plPieceMap (PlacedPiece pos (Block pattern)) =
    Map.fromList [ (rel <+> pos, BlockTile adjs) |
	rel <- pattern
	, let adjs = filter (\dir -> rel <+> dir `elem` pattern) hexDirs ]
plPieceMap (PlacedPiece pos (Pivot arms)) =
    let overarmed = length arms > 2 in
    Map.fromList $ (pos, PivotTile $ if overarmed then (head arms) else zero ) : 
	[ (rel <+> pos, ArmTile rel main) |
	    (rel,main) <- zip arms $ repeat False -- overarmed:$repeat False
	    ]
plPieceMap (PlacedPiece pos (Hook arm _)) =
    Map.fromList $ (pos, HookTile) : [ (arm <+> pos, ArmTile arm True) ]
plPieceMap (PlacedPiece pos (Wrench mom)) = Map.singleton pos $ WrenchTile mom
plPieceMap (PlacedPiece pos Ball) = Map.singleton pos BallTile

plPieceFootprint :: PlacedPiece -> [HexPos]
plPieceFootprint = Map.keys . plPieceMap

fullFootprint :: GameState -> PieceIdx -> [HexPos]
-- ^footprint of piece and connections ending at it
fullFootprint st idx = plPieceFootprint (getpp st idx) ++
    (concat $ map (connectionFootPrint st) $ springsEndAtIdx st idx)

footprintAt :: GameState -> PieceIdx -> [HexPos]
-- ^footprint of piece and any connections at it
footprintAt st idx = plPieceFootprint (getpp st idx) ++
    (concat $ map (connectionFootPrint st) $ springsAtIdx st idx)

collisions :: GameState -> PieceIdx -> PieceIdx -> [HexPos]
-- ^intersections of two pieces and their connections, disregarding
-- the connections which connect the two pieces
collisions st idx idx' =
    intersect (footprintAt st idx) (footprintAt st idx') \\
	(concat $ map (connectionFootPrint st)
	    $ filter (\conn ->
		Set.fromList [fst $ connectionRoot conn, fst $ connectionEnd conn] ==
		    Set.fromList [idx,idx'])
	    $ connections st)

connectionBoard :: GameState -> Connection -> GameBoard
connectionBoard st (Connection root end@(eidx,_) (Spring dir natLen)) =
    let rootPos = locusPos st root
	endPos = locusPos st end
	curLen = hexLen (endPos <-> rootPos) - 1
    in Map.fromList $
	[ ((d <*> dir) <+> rootPos, (eidx, SpringTile extension dir))
	    | d <- [1..curLen],
	    let extension | d <= natLen - curLen = Compressed
			  | curLen-d < 2*(curLen - natLen) = Stretched
			  | otherwise = Relaxed ]
connectionBoard _ _ = Map.empty

connectionFootPrint :: GameState -> Connection -> [HexPos]
connectionFootPrint s c = Map.keys $ connectionBoard s c

castRay :: HexPos -> HexDir -> GameBoard -> Maybe (PieceIdx, HexPos)
castRay start dir board =
    castRay' 30 start
    where castRay' 0 _ = Nothing
	  castRay' n pos =
	      case Map.lookup pos board of
		  Nothing -> castRay' (n-1) (dir<+>pos)
		  Just (idx,_) -> Just (idx,pos)

validGameState :: GameState -> Bool
validGameState st@(GameState pps conns) = and
    [ checkConnGraphAcyclic st
    , and [ null $ collisions st idx idx'
	    | idx <- [1..Vector.length pps - 1]
	    , idx' <- [0..idx-1] ]
    , and [ isHexDir dir
	    && castRay (dir<+>rpos) dir
		(stateBoard $ GameState pps (conns \\ [c]))
		== Just (eidx, epos)
	    && springExtensionValid st c
	    | c@(Connection root end@(eidx,_) (Spring dir _)) <- conns
	    , let [rpos,epos] = map (locusPos st) [root,end] ]
    , and [ 1 == length (components $ Set.fromList patt) 
	    | Block patt <- map placedPiece $ Vector.toList pps ]
    ]

	
modTile :: Maybe Tile -> HexPos -> HexPos -> Bool -> GameState -> GameState
modTile tile pos lastPos painting st =
    let board = stateBoard st
	curOwnedTile = Map.lookup pos board
	(st',mowner) = case curOwnedTile of
	    Nothing -> (st,Nothing)
	    Just (owner,SpringTile _ _) -> (delConnectionsIn pos st, Just owner)
	    Just (owner,_) -> delPiecePos owner pos st -- XXX may invalidate board's indices to st
	board' = stateBoard st'
	addPiece p = addpp $ PlacedPiece pos p
	lastMOwner = do
	    (o,_) <- Map.lookup lastPos board
	    return o
	{-
	same = isJust $ do
		t <- tile
		(_,t') <- curOwnedTile
		guard $ ((==) `on` tileType) t t'
		return $ Just ()
	-}
	lastElem os = isJust $ do
	       lastOwner <- lastMOwner
	       guard $ lastOwner `elem` os
	lastWasDiff = isNothing $ do
	       lastOwner <- lastMOwner
	       owner <- mowner
	       guard $ owner == lastOwner
	lastOK = painting || lastWasDiff
	validSpringRootTile ot = case snd ot of
	    BlockTile _ -> True
	    PivotTile _ -> True
	    _ -> False
	-- |Find next adjacent, skipping over current entity.
	-- XXX: Note this doesn't work when the entity is adjacent in multiple
	-- non-contiguous directions.
	nextOfAdjacents adjs loop = listToMaybe $ fromMaybe adjs $ do
		owner <- mowner
		i <- elemIndex owner adjs
		return $ (dropWhile (== owner) $ drop i adjs) ++
		    if loop && i > 0 then adjs else []
    in case mowner of
	 Just o | protectedPiece o -> st
	 _ -> (case tile of
	   -- _ | same && (pos /= lastPos) -> id
	   Just (BlockTile _) ->
	       let adjacentBlocks = [ idx |
		       dir <- hexDirs
		       , Just (idx, BlockTile _) <- [Map.lookup (dir <+> pos) board']
		       , not $ protectedPiece idx ]
		   addToIdx = if lastOK && lastElem adjacentBlocks
		       then lastMOwner
		       else nextOfAdjacents adjacentBlocks False
	       in case addToIdx of
		  Nothing -> addPiece $ Block [zero]
		  Just b -> addBlockPos b pos
	   Just (ArmTile armdir _) -> 
	       let adjacentPivots = [ idx |
		       dir <- if armdir == zero then hexDirs else [armdir, neg armdir]
		       , Just (idx, PivotTile _) <- [Map.lookup (dir <+> pos) board'] ]
		   addToIdx = if lastOK && lastElem adjacentPivots
		       then lastMOwner
		       else nextOfAdjacents adjacentPivots True
	       in case addToIdx of
		      Nothing -> id
		      Just p -> addPivotArm p pos
	   Just (SpringTile _ _) ->
	       let possibleSprings = [ Connection root end $ Spring sdir natLen |
		       sdir <- hexDirs
		       , let epos = sdir <+> pos
		       , Just (eidx, BlockTile _) <- [Map.lookup epos board']
		       , not $ protectedPiece eidx
		       , (ridx, rpos) <- maybeToList $ castRay (neg sdir <+> pos) (neg sdir) board'
		       , Just True == (validSpringRootTile `liftM` Map.lookup rpos board')
		       , let natLen = hexLen (rpos <-> epos) - 1
		       , natLen > 0
		       {-
		       , null [ conn |
			   conn@(Connection _ _ (Spring sdir' _)) <-
			       springsAtIdx st' eidx ++ springsEndAtIdx st' ridx
			   , not $ sdir' `elem` [sdir,neg sdir] ]
		       -}
		       , not $ connGraphPathExists st' eidx ridx
		       , let end = (eidx, epos <-> (placedPos $ getpp st' eidx))
		       , let root = (ridx, rpos <-> (placedPos $ getpp st' ridx))
		       ]
		   nextSpring = listToMaybe $ fromMaybe possibleSprings $ do
		       (_,SpringTile _ _) <- curOwnedTile -- XXX: therefore the indices of st are still valid
		       i <- findIndex (`elem` connections st) possibleSprings
		       return $ drop (i+1) possibleSprings
	       in case nextSpring of
		      Nothing -> id
		      Just conn -> addConn conn
	   Just (PivotTile _) -> addPiece $ Pivot []
	   Just (WrenchTile _) -> addPiece $ Wrench zero
	   Just HookTile -> let arm = listToMaybe [ dir | 
				    dir <- hexDirs
				    , isNothing $ Map.lookup (dir <+> pos) board' ]
			    in case arm of Just armdir -> addPiece $ Hook armdir NullHF
					   _ -> id
	   Just (BallTile) -> addPiece Ball
	   _ -> id
	   ) st'

mergeTiles :: HexPos -> HexDir -> Bool -> GameState -> GameState
mergeTiles pos dir mergePiece st = fromMaybe st $ do
    let board = stateBoard st
    (idx,tile) <- Map.lookup pos board
    (idx',tile') <- Map.lookup (dir<+>pos) board
    guard $ idx /= idx'
    guard $ all (not . protectedPiece) [idx,idx']
    case tile of
	BlockTile _ -> do
	    BlockTile _ <- Just tile'
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
