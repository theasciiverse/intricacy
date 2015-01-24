-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Init where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe
import System.Exit
import System.Environment
import System.Directory
import System.Console.GetOpt

import Mundanities
import AsciiLock
import Lock
import MainState
import Interact

data Opt = LockSize Int | ForceCurses
    deriving (Eq, Ord, Show)
options =
    [ Option ['c'] ["curses"] (NoArg ForceCurses) "force curses UI"
    -- , Option ['s'] ["locksize"] (ReqArg (LockSize . read) "SIZE") "locksize"
    ]
parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
	(o,n,[]) -> return (o,n)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: intricacy [OPTION...] [file]"

setup :: IO (Maybe Lock,[Opt],Maybe String)
setup = do
    argv <- getArgs
    (opts,args) <- parseArgs argv
    --let frameSize = fromMaybe 8 $ listToMaybe [ size | LockSize size <- opts ]
    (fromJust <$>) $ runMaybeT $ msum [ do
	rpath <- MaybeT $ return $ listToMaybe args
	msum [ do
	    path <- MaybeT $ (Just <$> canonicalizePath rpath)
			`catchIO` (const $ return Nothing)
	    lock <- reframe.fst <$> MaybeT (readLock path)
	    return (Just lock,opts,Just path)
	    , do
		lift $ putStrLn $ "Failed to open lock file: "++rpath
		lift $ exitFailure
		mzero ]
	, return (Nothing,opts,Nothing) ]

main' :: (UIMonad s, UIMonad c) =>
	Maybe (s MainState -> IO (Maybe MainState)) ->
	Maybe (c MainState -> IO (Maybe MainState)) -> IO ()
main' msdlUI mcursesUI = do
    (mlock,opts,mpath) <- setup
    initMState <- case mlock of
	    Nothing -> initMetaState
	    Just lock -> return $ newEditState lock Nothing mpath
    void $ runMaybeT $ msum [ do
	    finalState <- msum
		[ do
		    guard $ ForceCurses `notElem` opts
		    sdlUI <- MaybeT . return $ msdlUI
		    MaybeT $ sdlUI $ interactUI `execStateT` initMState
		, do
		    cursesUI <- MaybeT . return $ mcursesUI
		    MaybeT $ cursesUI $ interactUI `execStateT` initMState
		]
	    when (isNothing mlock) $ lift $ writeMetaState finalState
	    lift $ exitSuccess
	, lift exitFailure ]
