module Mundanities where
import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class
import Control.Exception
import System.IO.Error
import qualified Control.Exception as E
import System.Directory
import System.FilePath
import System.Environment
import Data.Maybe
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Paths_intricacy

-- | This is "catchIOError" in >=base-4.4, but we reimplement it to support
-- older bases.
catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = E.catch

readReadFile :: (Read a) => FilePath -> IO (Maybe a)
readReadFile file =
    ((fst <$>) . listToMaybe . reads . BSC.unpack <$> BS.readFile file)
	`catchIO` (const $ return Nothing)

writeReadFile :: (Show a) => FilePath -> a -> IO ()
writeReadFile file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ show x

confFilePath :: FilePath -> IO FilePath
confFilePath str =
    (++(pathSeparator:str)) <$> getAppUserDataDirectory "intricacy"

getDataPath :: FilePath -> IO FilePath
getDataPath = getDataFileName

makeConfDir :: IO ()
makeConfDir = confFilePath "" >>= createDirectoryIfMissing False

mkdirhierto = mkdirhier . takeDirectory
mkdirhier path = do
    exists <- doesDirectoryExist path
    when (not exists) $ mkdirhierto path >> createDirectory path

getDirContentsRec :: FilePath -> IO [FilePath]
getDirContentsRec path = flip catchIO (const $ return []) $ do
    contents <- map ((path++[pathSeparator])++) <$> filter ((/='.').head) <$> getDirectoryContents path
    annotated <- (\p -> (,) p <$> doesDirectoryExist p) `mapM` contents
    let (dirs,files) = join (***) (map fst) $ partition snd $ annotated
    (files++) . concat <$> getDirContentsRec `mapM` dirs
