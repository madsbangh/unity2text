{-# LANGUAGE OverloadedStrings #-}

module Unity.Meta (Guid, readMetaFileThrow) where

import Data.Foldable (find)
import Data.List (isPrefixOf, isSuffixOf)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO

type Guid = Text

readMetaFileThrow :: FilePath -> IO Guid
readMetaFileThrow path =
  if ".meta" `isSuffixOf` path
    then getGuidFromFile
    else error $ "Not a meta file: " ++ path
  where
    getGuidFromFile = do
      contents <- readFile' path
      let maybeGuidLine = find isGuidLine . lines $ contents
      return $ case maybeGuidLine of
        Just guidLine -> parseGuidLine guidLine
        Nothing -> error "Failed to find "
      where
        isGuidLine = ("guid: " `isPrefixOf`)
        parseGuidLine :: String -> Guid
        parseGuidLine = T.pack . drop 6