module Mundanities where
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import System.IO.Error
import System.Directory
import System.FilePath
import System.Environment
import Data.Maybe

readReadFile :: (Read a) => FilePath -> IO (Maybe a)
readReadFile file = do
    str <- liftIO $ catchJust (guard . isDoesNotExistError)
		  (readFile file)
		  (\_ -> return "")
    return $ length str `seq` -- XXX: forces file to be read and closed
	(liftM fst $ listToMaybe $ reads str)

confFilePath :: String -> IO FilePath
confFilePath str = do
    home <- getEnv "HOME"
    return $ home ++ "/.intricacy/"++str
    -- TODO: handle non-unix

makeConfDir :: IO ()
makeConfDir = confFilePath "" >>= createDirectoryIfMissing False

mkdirhier path = let dir = takeDirectory path in do
    exists <- doesDirectoryExist dir
    when (not exists) $ mkdirhier dir >> createDirectory dir

