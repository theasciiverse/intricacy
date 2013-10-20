module Lock where
import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map

import Frame
import GameState
import Hex
import Physics

type Lock = (Frame, GameState)
liftLock :: (GameState -> GameState) -> (Lock -> Lock)
liftLock g (f,st) = (f,g st)

lockSize (f,_) = frameSize f

deframe :: Lock -> Lock
deframe = delTools . liftLock (setpp 0 nullpp)
nullpp = PlacedPiece (PHS zero) (Block [])

reframe :: Lock -> Lock
reframe l@(f, st) = addTools $ delTools $ liftLock (setpp 0 (framePiece f)) l


type Solution = [PlayerMove]

checkSolution :: Lock -> Solution -> Bool
checkSolution lock pms =
    let (frame,st) = reframe lock
    in or $ map (\st' -> checkSolved (frame,st')) $
		scanl (((fst.runWriter).).flip physicsTick) st pms

checkSolved :: Lock -> Bool
checkSolved (f,st) =
    let b = stateBoard st in
	and [ Map.lookup p b == Nothing | p <- boltArea f ]


canonify :: Lock -> Lock
canonify = addTools . stabilise . delTools . delOOB
delTools :: Lock -> Lock
delTools = liftLock delTools'
    where
	delTools' :: GameState -> GameState
	delTools' st =
	    case listToMaybe [ idx | (idx,pp) <- enumVec $ placedPieces st
			      , isTool $ placedPiece pp ] of
		Nothing -> st
		Just idx -> delTools' $ delPiece idx st
addTools :: Lock -> Lock
addTools (f,st) =
    let st' = clearToolArea f st
    in (f, foldr addpp st' $ initTools f)
stabilise :: Lock -> Lock
stabilise = liftLock stabilise'
    where
	stabilise' :: GameState -> GameState
	stabilise' st =
	    let (st',_) = runWriter $ physicsTick NullPM st
	    in if st == st' then st else stabilise' st'
delOOB :: Lock -> Lock
delOOB l@(f,st) = case listToMaybe [ idx |
	(idx,_) <- enumVec $ placedPieces st
	, not $ isFrame idx
	, and $ map (not.inBounds f) $ fullFootprint st idx ] of
    Nothing -> l
    Just idx -> delOOB $ liftLock (delPiece idx) l
