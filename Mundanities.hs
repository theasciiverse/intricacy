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
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Control.Monad.Catch    (MonadMask, catch, handle)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Char8  as BSC
import           Data.List
import           Data.Maybe
import           Paths_intricacy
import           System.Directory
import           System.Environment     (getEnv)
import           System.FilePath

catchIO :: IO a -> (IOError -> IO a) -> IO a
catchIO = catch

ignoreIOErr :: (MonadIO m, MonadMask m, Monoid a) => m a -> m a
ignoreIOErr = handle ((\_ -> return mempty) :: (Monad m, Monoid a) => IOError -> m a)

ignoreIOErrAlt :: (MonadIO m, MonadMask m, Alternative f) => m (f a) -> m (f a)
ignoreIOErrAlt = handle ((\_ -> return empty) :: (Monad m, Alternative f) => IOError -> m (f a))

warnIOErrAlt :: (MonadIO m, MonadMask m, Alternative f) => m (f a) -> m (f a)
warnIOErrAlt = handle ((\e -> liftIO (print e) >> return empty) :: (MonadIO m, Alternative f) => IOError -> m (f a))

unlessIOErr :: (MonadIO m, MonadMask m) => m Bool -> m Bool
unlessIOErr = (fromMaybe False <$>) . ignoreIOErrAlt . (Just <$>)

readReadFile :: (Read a) => FilePath -> IO (Maybe a)
readReadFile file = ignoreIOErrAlt $ tryRead . BSC.unpack <$> BS.readFile file

tryRead :: (Read a) => String -> Maybe a
tryRead = (fst <$>) . listToMaybe . reads

readStrings :: FilePath -> IO [String]
readStrings file = ignoreIOErr $ lines . BSC.unpack <$> BS.readFile file

writeReadFile :: (Show a) => FilePath -> a -> IO ()
writeReadFile file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ show x

writeStrings :: FilePath -> [String] -> IO ()
writeStrings file x = do
    mkdirhierto file
    BS.writeFile file $ BSC.pack $ unlines x

confFilePath :: FilePath -> IO FilePath
confFilePath str = (++(pathSeparator:str)) <$>
    catchIO (getEnv "INTRICACY_PATH")
        (const $ getAppUserDataDirectory "intricacy")

getDataPath :: FilePath -> IO FilePath
getDataPath = getDataFileName

makeConfDir :: IO ()
makeConfDir = confFilePath "" >>= createDirectoryIfMissing False

fileExists :: FilePath -> IO Bool
fileExists = unlessIOErr . doesFileExist

mkdirhierto :: FilePath -> IO ()
mkdirhierto = mkdirhier . takeDirectory

mkdirhier :: FilePath -> IO ()
mkdirhier = createDirectoryIfMissing True

getDirContentsRec :: FilePath -> IO [FilePath]
getDirContentsRec path = ignoreIOErr $ do
    contents <- map ((path++[pathSeparator])++) . filter ((/='.').head) <$> getDirectoryContents path
    annotated <- (\p -> (,) p <$> doesDirectoryExist p) `mapM` contents
    let (dirs,files) = join (***) (map fst) $ partition snd annotated
    (files++) . concat <$> getDirContentsRec `mapM` dirs

fullLockPath path = if isAbsolute path
    then return path
    else do
        homePath <- getHomeDirectory
        locksPath <- confFilePath "locks"
        return $ if take 2 path == "~/"
            then homePath </> drop 2 path
            else locksPath </> path
