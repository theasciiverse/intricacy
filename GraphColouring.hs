module GraphColouring where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Vector as Vector
import Data.List
import Data.Maybe

type Colouring a = Map a Int
type Graph a = (Set a, Set (Set a))
type PlanarGraph a = Map a [a]

fourColour :: Ord a => Graph a -> Colouring a -> Colouring a
fourColour (nodes,edges) lastCol =
    -- bruteforce 
    if Map.keysSet lastCol == nodes && isColouring lastCol
	then lastCol
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
fiveColour g lastCol =
    if Map.keysSet lastCol == Map.keysSet g && isColouring lastCol
	then lastCol
	else fiveColour' g
    where
    isColouring mapping = and [
	Map.lookup s mapping /= Map.lookup e mapping |
	    s <- Map.keys g
	    , e <- g Map.! s ]
    --fiveColour' :: PlanarGraph a -> Colouring a
    fiveColour' g
	| g == Map.empty = Map.empty
	| otherwise =
	let 
	    adjsOf v = (nub $ g Map.! v) \\ [v]
	    v = head $ filter ((<=5) . length . adjsOf) $ Map.keys g
	    adjs = adjsOf v
	    addTo c = let vc = head $ possCols v \\ map (c Map.!) adjs
		      in Map.insert v vc c
	in if length adjs < 5
	   then addTo $ fiveColour' $ deleteNode v g
	   else let (v',v'') = if adjs!!2 `elem` (g Map.! (adjs!!0))
			then (adjs!!1,adjs!!3)
			else (adjs!!0,adjs!!2)
		in addTo $ demerge v' v'' $ fiveColour' $ merge v v' v'' g
    --possCols :: a -> [Int]
    possCols v = maybe [0..4] (\lvc -> lvc:([0..4] \\ [lvc])) $ Map.lookup v lastCol
    --demerge :: a -> a -> Colouring a -> Colouring a
    demerge v v' c = Map.insert v' (c Map.! v) c
    --merge :: a -> a -> a -> PlanarGraph a -> PlanarGraph a
    merge v v' v'' g =
	deleteNode v $ contractNodes v' v''
	    $ Map.adjust (concatAdjsOver v $ g Map.! v'') v' g
    --concatAdjsOver :: a -> [a] -> [a] -> [a]
    concatAdjsOver v adjs adjs' =
	let (s,_:e) = splitAt (fromJust $ elemIndex v adjs) adjs
	in s ++ adjs' ++ e
    deleteNode v =
	fmap (filter (/= v)) . Map.delete v
    contractNodes v v' =
	fmap (map (\v'' -> if v'' == v' then v else v'')) . Map.delete v'
    


