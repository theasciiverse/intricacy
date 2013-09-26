module Init where


import Control.Monad
import Data.Maybe
import Control.Exception
import System.IO.Error
import System.Environment
import System.Console.GetOpt

import Mundanities
import Frame
import BinaryInstances
import Lock

data Opt = LockSize Int | ForceCurses
    deriving (Eq, Ord, Show)
options = [
    Option ['s'] ["locksize"] (ReqArg (LockSize . read) "SIZE") "locksize"
    , Option ['c'] ["curses"] (NoArg ForceCurses) "force curses UI"
    ]
parseArgs :: [String] -> IO ([Opt],[String])
parseArgs argv =
    case getOpt Permute options argv of
	(o,n,[]) -> return (o,n)
	(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: lock [OPTION...] [file]"

readLockFile :: String -> IO (Maybe Lock)
readLockFile = readReadFile

init :: IO (Lock,[Opt],Maybe String)
init = do
    argv <- getArgs
    (opts,args) <- parseArgs argv
    let frameSize = fromMaybe 8 $ listToMaybe [ size | LockSize size <- opts ]
    let initFrame = BasicFrame frameSize
    let fname = listToMaybe args
    mlock <- case fname of
		Nothing -> return Nothing
		Just fname -> readLockFile fname
    let lock = fromMaybe (initFrame, baseState initFrame) $ liftM reframe mlock
    return (lock,opts,fname)
