{-# LANGUAGE OverloadedStrings #-}

module GuidNameLookup where

import Data.List (isSuffixOf)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.Directory.Recursive (getFilesRecursive)
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import Unity.Meta (Guid, readMetaFileThrow)

assetLookupFile :: FilePath
assetLookupFile = ".git/unity2text/lookup"

createAssetNameLookup :: FilePath -> IO (M.Map Guid Text)
createAssetNameLookup rootPath = do
  putStrLn "Building lookup..."
  metaPaths <- getPathsRecursiveWhere (".meta" `isSuffixOf`) rootPath
  guids <- mapM readMetaFileThrow metaPaths
  putStrLn "Done."
  return $ M.fromList $ zip guids (map (T.pack . takeBaseName) metaPaths)

getPathsRecursiveWhere :: (FilePath -> Bool) -> FilePath -> IO [FilePath]
getPathsRecursiveWhere predicate dir =
  filter predicate <$> getFilesRecursive dir

saveAssetNameLookup :: M.Map Guid Text -> IO ()
saveAssetNameLookup m = do
  isInRepoRoot <- doesDirectoryExist ".git"
  if isInRepoRoot
    then go
    else do
      putStrLn "Not in repo root. Aborting."
      exitFailure
  where
    go = do
      let pairs = M.toAscList m
      let guidNameLines = map (\(x, y) -> x <> " " <> y) pairs
      _ <- createDirectoryIfMissing False ".git/unity2text"
      T.writeFile assetLookupFile $ T.unlines guidNameLines

-- Expects file to be a list of guid + name pairs, separated by a space, distinct and ascending
loadAssetNameLookup :: IO (M.Map Guid Text)
loadAssetNameLookup = do
  exists <- doesFileExist assetLookupFile
  if exists
    then do
      contents <- T.readFile assetLookupFile
      let guidNameLines = T.lines contents
      let pairs = map (\l -> (T.takeWhile (/= ' ') l, T.drop 1 . T.dropWhile (/= ' ') $ l)) guidNameLines
      return $ M.fromDistinctAscList pairs
    else return M.empty
