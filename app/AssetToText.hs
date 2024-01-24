{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module AssetToText where

import Context
import Control.Applicative ((<|>))
import qualified Data.Aeson.KeyMap as K
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Either.Extra (fromRight)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Vector as V
import Data.Yaml (parseMaybe)
import qualified Data.Yaml as Y
import Unity.AssetModel
  ( BuiltInClass (FullBuiltInClass),
    FileId,
    GameObject,
    MonoBehaviour (..),
    PrefabInstance (FullPrefabInstance, prefabInstanceTypeName),
    Transform (..),
    TransformType (..),
    UnityObject (..),
    name,
    parent,
    source,
    transformType,
    typeName,
    unityObjectGameObject,
  )
import Unity.Meta (Guid)

prettyPrintAsset :: Context -> [(FileId, UnityObject)] -> IO ()
prettyPrintAsset context = mapM_ printObj
  where
    printObj :: (FileId, UnityObject) -> IO ()
    printObj (fid, obj) =
      let goPath = gameObjectPath context $ unityObjectGameObject fid obj
       in printObjectWith goPath context fid

printObjectWith :: T.Text -> Context -> FileId -> IO ()
printObjectWith goPath context fid =
  case getObject fid context of
    Nothing -> mempty
    Just obj -> do
      mapM_ putLine . BS.lines . replaceObjectReferencesInSource context . source $ obj
      where
        putLine :: ByteString -> IO ()
        putLine line = do
          BS.putStrLn . encodeUtf8 $
            goPath
              <> "."
              <> niceObjectName context obj
              <> ": "
              <> decodeUtf8 line

niceObjectName :: Context -> UnityObject -> T.Text
niceObjectName _ (GameObject _ _) = "GameObject"
niceObjectName _ (Transform (FullTransform {transformType = NormalTransform}) _) = "Transform"
niceObjectName _ (Transform (FullTransform {transformType = RectTransform}) _) = "RectTransform"
niceObjectName context (MonoBehaviour FullMonoBehaviour {scriptGuid = g} _) = monoBehaviourName g context
niceObjectName _ (BuiltInClass (FullBuiltInClass {typeName = n}) _) = n
niceObjectName _ (PrefabInstance (FullPrefabInstance {prefabInstanceTypeName = n}) _) = n <> " (Prefab Instance)"

replaceObjectReferencesInSource :: Context -> ByteString -> ByteString
replaceObjectReferencesInSource context src =
  fromRight src $ do
    obj <- Y.decodeEither' src
    let obj' = replaceObjectReferences context obj
    return $ Y.encode obj'

replaceObjectReferences :: Context -> Y.Object -> Y.Object
replaceObjectReferences context = K.map $ \v -> fromMaybe (fallback v) (parseMaybe (parseObjectReference context) v)
  where
    fallback :: Y.Value -> Y.Value
    fallback (Y.Array a) = Y.Array (V.map fallback a)
    fallback (Y.Object o) = Y.Object (replaceObjectReferences context o)
    fallback a = a

parseObjectReference :: Context -> Y.Value -> Y.Parser Y.Value
parseObjectReference context =
  Y.withObject "ObjectReference" $ \o ->
    do
      fid <- o Y..: "fileID" :: Y.Parser FileId
      maybeGuid <- o Y..:? "guid" :: Y.Parser (Maybe Guid)
      return $ makePrettyObjectReference context fid maybeGuid

makePrettyObjectReference :: Context -> FileId -> Maybe Guid -> Y.Value
makePrettyObjectReference context fid maybeGuid = fromMaybe
  (Y.String "(Unknown)")
  $ case (fid, maybeGuid) of
    (0, Nothing) -> return $ Y.String "(None)"
    (0, Just g) -> do
      n <- getName g context <|> pure "(Unknown)"
      return (Y.String n)
    (f, Nothing) -> do
      o <- getObject f context
      let goPath = gameObjectPath context $ unityObjectGameObject fid o
      return (Y.String $ goPath <> "." <> niceObjectName context o)
    (f, Just g) -> do
      n <- getName g context <|> pure "(Unknown)"
      return (Y.String $ (T.pack . show $ f) <> " in " <> n)

monoBehaviourName :: Guid -> Context -> T.Text
monoBehaviourName g context = fromMaybe unknownScript (getName g context)
  where
    unknownScript = "(Unknown Script " <> g <> ")"

gameObjectPath :: Context -> FileId -> T.Text
gameObjectPath context = path
  where
    path :: FileId -> T.Text
    path childGameObjectId = fromMaybe "(Root)" $ do
      childName <- name <$> getGameObject childGameObjectId
      return $ case path <$> parentGameObjectId childGameObjectId of
        Just p -> p <> "/" <> childName
        Nothing -> childName

    parentGameObjectId :: FileId -> Maybe FileId
    parentGameObjectId childGameObjectId = do
      childTransformId <- getTransformId childGameObjectId context
      childTransform <- getTransform childTransformId
      let parentTransformId = parent childTransform
      parentTransform <- getTransform parentTransformId
      return $ transformGameObject parentTransform

    getGameObject :: FileId -> Maybe GameObject
    getGameObject fid = do GameObject o _ <- getObject fid context; return o

    getTransform :: FileId -> Maybe Transform
    getTransform fid = do Transform t _ <- getObject fid context; return t
