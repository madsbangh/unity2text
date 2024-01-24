{-# LANGUAGE OverloadedStrings #-}

module Unity.AssetParsing where

import Data.Aeson.KeyMap (elems)
import qualified Data.Aeson.KeyMap as K
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Data.DList (DList)
import qualified Data.DList as D
import Data.Map (Map, fromList)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Data.Word (Word8)
import Data.Yaml (Object, Value (Object), decodeThrow)
import qualified Data.Yaml as Y
import Text.Read (readMaybe)
import Unity.AssetModel
import Unity.TypeIds
  ( gameObject,
    monoBehaviour,
    rectTransform,
    transform,
  )

readAssetFile :: FilePath -> IO [(FileId, UnityObject)]
readAssetFile f = do
  fileContent <- B.readFile f
  let parts = splitAsset fileContent
  mapM parsePart parts

type Part = (TypeId, FileId, ByteString, Bool)

splitAsset :: ByteString -> [Part]
splitAsset = D.toList . go D.empty . dropHeader . toLines
  where
    go :: DList Part -> [ByteString] -> DList Part
    go acc [] = acc
    go acc (line : rest)
      | isTagLine && isStripped = go (acc `D.snoc` (tid, fid, yaml, True)) linesAfterYaml
      | isTagLine = go (acc `D.snoc` (tid, fid, yaml, False)) linesAfterYaml
      | otherwise = error . C.unpack $ "ERROR: Unrecognised line: " <> line
      where
        isTagLine :: Bool
        isTagLine = "--- !u!" `B.isPrefixOf` line

        isStripped :: Bool
        isStripped = "stripped" `B.isSuffixOf` line

        tid :: Int
        tid = fromMaybe (-1) . readMaybe . takeWhile isDigit . drop 7 . C.unpack $ line

        fid :: Int
        fid = fromMaybe (-1) . readMaybe . drop 2 . dropWhile isDigit . drop 7 . C.unpack $ line

        yaml :: ByteString
        yaml = fromLines yamlLines

        yamlLines :: [ByteString]
        yamlLines = takeWhile (not . ("--- !u!" `B.isPrefixOf`)) rest

        linesAfterYaml :: [ByteString]
        linesAfterYaml = drop (length yamlLines) rest

toLines :: ByteString -> [ByteString]
toLines = B.split newline

fromLines :: [ByteString] -> ByteString
fromLines = B.intercalate "\n"

newline :: Word8
newline = 0xA

-- We drop these two lines at the top of a typical Unity YAML asset
-- %YAML 1.1
-- %TAG !u! tag:unity3d.com,2011:
dropHeader :: [a] -> [a]
dropHeader = drop 2

parsePart :: Part -> IO (FileId, UnityObject)
parsePart (tid, fid, yaml, False) = do
  obj <- makeUnityObject tid yaml yaml
  return (fid, obj)
parsePart (tid, fid, yaml, True) = do
  obj <- makeStrippedUnityObject tid yaml yaml
  return (fid, obj)

makeStrippedUnityObject :: TypeId -> ByteString -> ByteString -> IO UnityObject
makeStrippedUnityObject _ yaml yamlSource = parsedObject
  where
    parsedObject = do
      piData <- decodeThrow yaml
      return $ PrefabInstance piData yamlSource

makeUnityObject :: Int -> ByteString -> ByteString -> IO UnityObject
makeUnityObject tid yaml yamlSource
  | tid == gameObject = parsedGameObject
  | tid == transform = parsedTransform
  | tid == rectTransform = parsedTransform
  | tid == monoBehaviour = parsedMonoBehaviour
  | otherwise = parsedBuiltInClass
  where
    parsedGameObject :: IO UnityObject
    parsedGameObject = do
      let godata = case Y.decodeEither' yaml of
            Right success -> return success
            Left _ -> FullGameObject . gameObjectNameFallback <$> Y.decodeThrow yaml
      GameObject <$> godata <*> pure yamlSource
    parsedTransform = do
      tData <- case Y.decodeEither' yaml of
        Right success -> return success
        Left e -> error ("Failed to parse transform\n" ++ C.unpack yaml ++ "\n" ++ Y.prettyPrintParseException e)
      return $ Transform tData yamlSource
    parsedMonoBehaviour = do
      mbData <- decodeThrow yaml
      return $ MonoBehaviour mbData yamlSource
    parsedBuiltInClass = do
      cData <- decodeThrow yaml
      return $ BuiltInClass cData yamlSource

createObjectLookup :: [(FileId, UnityObject)] -> Map FileId UnityObject
createObjectLookup asset =
  fromList $ do
    (fid, obj) <- asset
    return (fid, obj)

unwrap :: Y.Object -> Maybe Y.Object
unwrap outer = do
  Object inner <- listToMaybe . elems $ outer
  return inner

gameObjectNameFallback :: Object -> T.Text
gameObjectNameFallback go =
  case unwrap go >>= K.lookup "m_Name" of
    Just (Y.String s) -> s
    Just (Y.Bool b) -> if b then "Yes" else "No" -- Yes/No is parsed as Bool by Yaml parser. Fall back to best guess for now
    _ -> "(Unnamed)"
