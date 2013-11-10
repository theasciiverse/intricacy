module Init where


import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Data.Maybe
import System.Exit
import System.Environment
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
    let fname = listToMaybe args
    mlock <- runMaybeT $ (MaybeT . return) fname >>= MaybeT . readLock
    return (liftM reframe mlock,opts,fname)

main' :: (UIMonad s, UIMonad c) =>
	Maybe (s MainState -> IO (Maybe MainState)) ->
	Maybe (c MainState -> IO (Maybe MainState)) -> IO ()
main' msdlUI mcursesUI = do
    (mlock,opts,mpath) <- setup
    initMState <- case mlock of
	    Nothing -> initMetaState
	    Just lock -> return $ newEditState lock mpath
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
