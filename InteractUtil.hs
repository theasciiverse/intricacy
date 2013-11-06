module InteractUtil where

import Control.Monad.State
import Control.Applicative
import qualified Data.Vector as Vector
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Monad.Writer
import Control.Monad.Trans.Error
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Char
import Data.List
import System.Directory
import Data.Array
import Data.Function (on)

import Hex
import Command
import Physics
import Mundanities
import GameState
import Frame
import Lock
import Protocol
import Metagame
import MainState
import InputMode

checkWon :: UIMonad uiM => StateT MainState uiM ()
checkWon = do
    st <- gets psCurrentState
    frame <- gets psFrame
    when (checkSolved (frame,st)) $ do
	modify $ \ps -> ps {psSolved = True}
	lift $ drawMessage "Unlocked!"
	{-
	pms <- (reverse.(map snd)) `liftM` gets psGameStateMoveStack
	let wfname = fromMaybe "unnamed.lock" fname 
	liftIO $ writeFile (wfname++".solution") $ show pms
	liftIO $ B.writeFile (wfname++".solution.bin") $ Bin.encode $ pms
	-}
doForce force = do
    st:_ <- gets esGameStateStack
    let (st',alerts) = runWriter $ resolveForces
	    (Vector.singleton (ForceChoice [force])) Vector.empty
	    (\_ _->False) st
    lift (drawAlerts alerts) >> pushEState st'
drawTile pos tile painting = do
    modify $ \es -> es {selectedPiece = Nothing}
    lastMP <- gets lastModPos
    modifyEState $ modTile tile pos lastMP painting
    modify $ \es -> es {lastModPos = pos}
paintTilePath tile from to = if from == to
then modify $ \es -> es {lastModPos = to}
else let from' = (hexVec2HexDirOrZero $ to<->from) <+> from
    in drawTile from' tile True >> paintTilePath tile from' to

pushEState :: UIMonad uiM => GameState -> StateT MainState uiM ()
pushEState st = do
    st':sts <- gets esGameStateStack
    when (st' /= st) $ modify $ \es -> es {esGameStateStack = st:st':sts, esUndoneStack = []}
pushPState :: UIMonad uiM => (GameState,PlayerMove) -> StateT MainState uiM ()
pushPState (st,pm) = do
    st' <- gets psCurrentState
    stms <- gets psGameStateMoveStack
    when (st' /= st) $ modify $ \ps -> ps {psCurrentState = st,
	    psGameStateMoveStack = (st',pm):stms, psUndoneStack = []}
modifyEState :: UIMonad uiM => (GameState -> GameState) -> StateT MainState uiM ()
modifyEState f = do
    st:_ <- gets esGameStateStack
    pushEState $ f st

doPhysicsTick :: UIMonad uiM => PlayerMove -> GameState -> uiM (GameState, [Alert])
doPhysicsTick pm st =
    let r@(st',alerts) = runWriter $ physicsTick pm st in
    drawAlerts alerts >> return r

nextLock :: Bool -> FilePath -> IO FilePath
nextLock newer path = do
    lockdir <- confFilePath "locks"
    time <- (Just <$> (fullLockPath path >>= getModificationTime))
	`catchIO` const (return Nothing)
    paths <- getDirContentsRec lockdir
    maybe path (drop (length lockdir + 1) . fst) . listToMaybe .
	(if newer then id else reverse) . sortBy (compare `on` snd) .
	filter (maybe (const True)
	    (\x y -> (if newer then (<) else (>)) x (snd y)) time) <$>
	(\p -> (,) p <$> getModificationTime p) `mapM` paths

setLockPath path = do
    lock <- liftIO $ readLock path
    modify $ \ms -> ms {curLockPath = path, curLock = lock}

declare undecl@(Undeclared soln ls al) = do
    ourName <- mgetOurName
    ourUInfo <- mgetUInfo ourName
    idx <- askLockIndex "Secure behind which lock?" "You first need to place a lock to secure your solution behind." (\i -> isJust $ userLocks ourUInfo ! i)
    guard $ isJust $ userLocks ourUInfo ! idx
    resp <- lift $ curServerAction $ DeclareSolution soln ls al idx
    case resp of
	ServerAck -> lift $ do
	    modify $ \ms -> ms {undeclareds = delete undecl $ undeclareds ms}
	    mapM_ invalidateUInfo [lockOwner al, ourName]
	ServerError err -> do
	    lift.lift $ drawError err
	    mzero
	_ -> return ()

askLockIndex prompt failMessage pred = do
    let ok = filter pred [0,1,2]
    case length ok of
	0 -> (lift.lift) (drawError failMessage) >> mzero
	1 -> return $ head ok
	_ -> ask ok
    where
	ask ok = do
	    let prompt' = prompt ++ " [" ++ intersperse ',' (map lockIndexChar ok) ++ "]"
	    idx <- MaybeT $ lift $ join . (((charLockIndex<$>).listToMaybe)<$>) <$>
		textInput prompt' 1 True Nothing Nothing
	    if idx `elem` ok then return idx else ask ok
confirmOrBail prompt = (guard =<<) $ lift.lift $ confirm prompt
confirm :: UIMonad uiM => String -> uiM Bool
confirm prompt = do
    drawMessage $ prompt ++ " [y/N]"
    waitConfirm <* drawMessage ""
    where
	waitConfirm = do
	    cmds <- getInput IMTextInput
	    case msum $ map ansOfCmd cmds of
		Just answer -> return answer
		Nothing -> waitConfirm
	ansOfCmd (CmdInputChar 'n') = Just False
	ansOfCmd (CmdInputChar 'N') = Just False
	ansOfCmd (CmdInputChar 'y') = Just True
	ansOfCmd (CmdInputChar 'Y') = Just True
	ansOfCmd (CmdInputChar '\ESC') = Just False
	ansOfCmd _ = Nothing

-- | TODO: draw cursor
textInput :: UIMonad uiM => String -> Int -> Bool -> Maybe [String] -> Maybe String -> uiM (Maybe String)
textInput prompt maxlen endOnMax mposss init = getText (fromMaybe "" init,"") <* drawMessage ""
    where
	getText (s,stem) = do
	    drawMessage $ prompt ++ " " ++ s
	    if endOnMax && maxlen == length s
	    then return $ Just s
	    else do
		cmds <- getInput IMTextInput
		case foldM applyCmd (s,stem) cmds of
		    Left False -> return Nothing
		    Left True -> return $ Just s
		    Right (s',stem') -> getText (s',stem')
	    where
		applyCmd (s,stem) (CmdInputChar c) = case c of
		    '\ESC' -> Left False
		    '\n' -> Left True
		    '\r' -> Left True
		    '\NAK' -> Right ("","")  -- ^U
		    '\b' -> Right $ (take (length s - 1) s, "")
		    '\DEL' -> Right $ (take (length s - 1) s, "")
		    '\t' -> case mposss of
			Nothing -> Right (s,stem)
			Just possibilities -> if null stem
			    then let
				completions = filter (completes s) possibilities
				pref = if null completions then s else
				    let c = head completions
				    in head [ c' | n <- reverse [0..length c],
					let c'=take n c, all (completes c') completions ]
				in Right (pref,pref)
			    else let
				completions = filter (completes stem) possibilities
				later = filter (>s) completions
				s' | null completions = s
				   | null later = head completions
				   | otherwise = minimum later
				in Right (s',stem)
		    _ -> Right $ if isPrint c
			    then ((if length s >= maxlen then id else (++[c])) s, "")
			    else (s,stem)
		applyCmd x CmdRedraw = Right x
		applyCmd _ _ = Left False
	completes s s' = take (length s) s' == s
