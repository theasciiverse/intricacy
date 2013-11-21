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
import Util
import GameStateTypes
--import Debug

ppidxs :: GameState -> [PieceIdx]
ppidxs = Vector.toList . (Vector.findIndices $ const True) . placedPieces

getpp :: GameState -> PieceIdx -> PlacedPiece
getpp st idx = (placedPieces st) ! idx

setpp :: PieceIdx -> PlacedPiece -> GameState -> GameState
setpp idx pp st@(GameState pps _) = 
    let displacement = (placedPos $ getpp st idx) <-> placedPos pp
	updateConn conn@(Connection root@(ridx,rpos) end@(eidx,epos) link)
	    | ridx == idx = Connection (ridx,rpos<+>displacement) end link
	    | eidx == idx = Connection root (eidx,epos<+>displacement) link
	    | otherwise = conn
    in st {placedPieces = pps // [(idx, pp)]
	, connections = map updateConn $ connections st }

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

setPiece :: PieceIdx -> Piece -> GameState -> GameState
setPiece idx p st = 
    setpp idx (PlacedPiece (placedPos $ getpp st idx) p) st

adjustPieces :: (Piece -> Piece) -> GameState -> GameState
adjustPieces f st =
    st { placedPieces = fmap
	(\pp -> pp { placedPiece = f $ placedPiece pp })
       $ placedPieces st }

addBlockPos :: PieceIdx -> HexPos -> GameState -> GameState
addBlockPos b pos st =
    let PlacedPiece ppos (Block patt) = getpp st b
    in setPiece b (Block (pos <-> ppos:patt)) st

addPivotArm :: PieceIdx -> HexPos -> GameState -> GameState
addPivotArm p pos st =
    let PlacedPiece ppos (Pivot arms) = getpp st p
    in setPiece p (Pivot (pos <-> ppos:arms)) st

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

springsAtIdx,springsEndAtIdx,springsRootAtIdx :: GameState -> PieceIdx -> [Connection]
springsAtIdx st idx =
    [ c | c@(Connection (ridx,_) (eidx, _) (Spring _ _)) <- connections st
	  , idx `elem` [ridx,eidx] ]
springsAtIdxIgnoring st idx idx' =
    [ c | c@(Connection (ridx,_) (eidx, _) (Spring _ _)) <- connections st
	  , idx `elem` [ridx,eidx], idx' `notElem` [ridx,eidx] ]
springsEndAtIdx st idx =
    [ c | c@(Connection _ (eidx, _) (Spring _ _)) <- connections st
	  , eidx==idx ]
springsRootAtIdx st idx =
    [ c | c@(Connection (ridx, _) _ (Spring _ _)) <- connections st
	  , ridx==idx ]
connectionsBetween :: GameState -> PieceIdx -> PieceIdx -> [Connection]
connectionsBetween st idx idx' =
    filter connIsBetween $ connections st
    where
	connIsBetween conn =
	    isPerm (idx,idx') (fst $ connectionRoot conn, fst $ connectionEnd conn)
	isPerm (x,y) (x',y') = (x==x'&&y==y')||(x==y'&&y==x')

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
    let idxs = ppidxs st
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
	    let (st',midx) = componentify idx $ setpp idx (PlacedPiece ppos $ Block $ patt \\ [v]) st
	    in (repossessConns st st', midx)
        Pivot arms -> if v == zero
	   then (delPiece idx st, Nothing)
	   else ((setPiece idx $ Pivot $ arms \\ [v]) st, Just idx)
        _ -> (delPiece idx st, Nothing)
componentify :: PieceIdx -> GameState -> (GameState, Maybe PieceIdx)
componentify idx st = let PlacedPiece ppos p = getpp st idx
    in case p of
        Block patt ->
	    let comps = components $ Set.fromList patt
		ppOfComp (v,patt) = PlacedPiece (v<+>ppos) $ Block $ Set.toList patt
	    in case comps of
		[] -> (delPiece idx st, Nothing)
		zeroComp:newComps ->
		    (setpp idx (ppOfComp zeroComp)
			$ foldr (addpp . ppOfComp) st newComps, Just idx)
	_ -> (st,Nothing)

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

footprintAtIgnoring :: GameState -> PieceIdx -> PieceIdx -> [HexPos]
-- ^footprint of piece and any connections at it, except those with idx'
footprintAtIgnoring st idx idx' = plPieceFootprint (getpp st idx) ++
    (concat $ map (connectionFootPrint st) $ springsAtIdxIgnoring st idx idx')

collisions :: GameState -> PieceIdx -> PieceIdx -> [HexPos]
-- ^intersections of two pieces and their connections, disregarding
-- the connections which connect the two pieces
collisions st idx idx' =
    intersect (footprintAt st idx) (footprintAt st idx') \\
	(concat $ map (connectionFootPrint st) $ connectionsBetween st idx idx')

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
    [ checkValidHex st
    , checkConnGraphAcyclic st
    , and [ null $ collisions st idx idx'
	    | idx <- ppidxs st
	    , idx' <- [0..idx-1] ]
    , and [ isHexDir dir
	    && castRay (dir<+>rpos) dir
		(stateBoard $ GameState pps (conns \\ [c]))
		== Just (eidx, epos)
	    && springExtensionValid st c
	    && (validRootPiece $ placedPiece $ pps ! ridx)
	    && (validEndPiece $ placedPiece $ pps ! eidx)
	    | c@(Connection root@(ridx,_) end@(eidx,_) (Spring dir _)) <- conns
	    , let [rpos,epos] = map (locusPos st) [root,end] ]
    , and [ 1 == length (components $ Set.fromList patt) 
	    | Block patt <- map placedPiece $ Vector.toList pps ]
    ]

validRootPiece (Block _) = True
validRootPiece (Pivot _) = True
validRootPiece _ = False
validEndPiece (Block _) = True
validEndPiece _ = False

checkValidHex (GameState pps conns) = and
    [ all validPP $ Vector.toList pps
    , all validConn conns ]
    where
	validVec (HexVec x y z) = x+y+z==0
	validPos (PHS v) = validVec v
	validDir v = validVec v && isHexDir v
	validPP (PlacedPiece pos piece) = validPos pos && validPiece piece
	validPiece (Block patt) = all validVec patt
	validPiece (Pivot arms) = all validDir arms
	validPiece (Hook dir _) = validDir dir
	validPiece _ = True
	validConn (Connection (_,rv) (_,ev) link) = all validVec [rv,ev] && validLink link
	validLink (Free v) = validVec v
	validLink (Spring dir _) = validDir dir

protectedPiece :: PieceIdx -> Bool
protectedPiece = isFrame
isFrame :: PieceIdx -> Bool
isFrame = (==0)
