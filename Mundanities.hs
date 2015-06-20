-- This file is part of Intricacy
-- Copyright (C) 2013 Martin Bays <mbays@sdf.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of version 3 of the GNU General Public License as
-- published by the Free Software Foundation, or any later version.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.

module Mundanities where
import Control.Applicative
import Control.Arrow
import Control.Monad
import qualified Control.Exception as E
import System.Directory
import System.FilePath
import Data.Maybe
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Paths_intricacy

-- | This is "catchIOError" in >=base-4.4, but we reimplement it to support
-- older bases.
catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = E.catch

catchAll :: IO a -> (E.SomeException -> IO a) -> IO a
catchAll = E.catch

readReadFile :: (Read a) => FilePath -> IO (Maybe a)
readReadFile file =
    (tryRead . BSC.unpack <$> BS.readFile file)
	`catchIO` (const $ return Nothing)
tryRead :: (Read a) => String -> Maybe a
tryRead = (fst <$>) . listToMaybe . reads

readStrings :: FilePath -> IO [String]
readStrings file = (lines . BSC.unpack) <$> BS.readFile file

writeReadFile :: (Show a) => FilePath -> a -> IO ()
writeReadFile file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ show x

writeStrings :: FilePath -> [String] -> IO ()
writeStrings file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ unlines x

confFilePath :: FilePath -> IO FilePath
confFilePath str =
    (++(pathSeparator:str)) <$> getAppUserDataDirectory "intricacy"

getDataPath :: FilePath -> IO FilePath
getDataPath = getDataFileName

makeConfDir :: IO ()
makeConfDir = confFilePath "" >>= createDirectoryIfMissing False

mkdirhierto :: FilePath -> IO ()
mkdirhierto = mkdirhier . takeDirectory

mkdirhier :: FilePath -> IO ()
mkdirhier path = do
    exists <- doesDirectoryExist path
    unless exists $ mkdirhierto path >> createDirectory path

getDirContentsRec :: FilePath -> IO [FilePath]
getDirContentsRec path = flip catchIO (const $ return []) $ do
    contents <- map ((path++[pathSeparator])++) <$> filter ((/='.').head) <$> getDirectoryContents path
    annotated <- (\p -> (,) p <$> doesDirectoryExist p) `mapM` contents
    let (dirs,files) = join (***) (map fst) $ partition snd annotated
    (files++) . concat <$> getDirContentsRec `mapM` dirs

fullLockPath path = if isAbsolute path
    then return path
    else (++(pathSeparator:path)) <$> confFilePath "locks"
