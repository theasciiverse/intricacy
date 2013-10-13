module Mundanities where
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import System.IO.Error
import System.Directory
import System.FilePath
import System.Environment
import Data.Maybe
import Data.List

readReadFile :: (Read a) => FilePath -> IO (Maybe a)
readReadFile file = do
    str <- liftIO $ catchJust (guard . isDoesNotExistError)
		  (readFile file)
		  (\_ -> return "")
    return $ length str `seq` -- XXX: forces file to be read and closed
	(liftM fst $ listToMaybe $ reads str)

confFilePath :: String -> IO FilePath
confFilePath str =
    (++(pathSeparator:str)) <$> getAppUserDataDirectory "intricacy"

makeConfDir :: IO ()
makeConfDir = confFilePath "" >>= createDirectoryIfMissing False

mkdirhierto = mkdirhier . takeDirectory
mkdirhier path = do
    exists <- doesDirectoryExist path
    when (not exists) $ mkdirhierto path >> createDirectory path

getDirContentsRec :: FilePath -> IO [FilePath]
getDirContentsRec path = flip catchIOError (const $ return []) $ do
    contents <- map ((path++[pathSeparator])++) <$> filter ((/='.').head) <$> getDirectoryContents path
    annotated <- (\p -> (,) p <$> doesDirectoryExist p) `mapM` contents
    let (dirs,files) = (\(x,y) -> (map fst x, map fst y)) $ partition snd $ annotated
    (files++) . concat <$> getDirContentsRec `mapM` dirs
