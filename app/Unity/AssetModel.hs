{-# LANGUAGE OverloadedStrings #-}

module Unity.AssetModel where

import Control.Applicative ((<|>))
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as K
import Data.ByteString
import Data.Text
import Data.Yaml
import Unity.Meta (Guid)

type FileId = Int

type TypeId = Int

data UnityObject
  = GameObject GameObject ByteString
  | Transform Transform ByteString
  | MonoBehaviour MonoBehaviour ByteString
  | BuiltInClass BuiltInClass ByteString
  | PrefabInstance PrefabInstance ByteString
  deriving (Show)

data GameObject
  = FullGameObject {name :: Text}
  | StrippedGameObject
      {gameObjectPrefabInstance :: FileId}
  deriving (Show)

data Transform
  = FullTransform
      { transformType :: TransformType,
        parent :: FileId,
        transformGameObject :: FileId
      }
  | StrippedTransform
      {transformPrefabInstance :: FileId}
  deriving (Show)

data TransformType = NormalTransform | RectTransform deriving (Show)

data MonoBehaviour
  = FullMonoBehaviour
      {scriptGuid :: Guid, monoBehaviourGameObject :: FileId}
  | StrippedMonoBehaviour
      {scriptGuid :: Guid, monoBehaviourPrefabInstance :: FileId}
  deriving (Show)

data BuiltInClass
  = FullBuiltInClass
      { typeName :: Text,
        builtInClassGameObject :: FileId
      }
  | StrippedBuiltInClass BuiltInClass
  deriving (Show)

data PrefabInstance
  = FullPrefabInstance
  { prefabInstanceTypeName :: Text,
    correspondingSourceObject :: Guid,
    prefabInstance :: FileId
  }
  deriving (Show)

unityObjectGameObject :: FileId -> UnityObject -> FileId
unityObjectGameObject self (GameObject _ _) = self
unityObjectGameObject _ (Transform t _) = transformGameObject t
unityObjectGameObject _ (MonoBehaviour mb _) = monoBehaviourGameObject mb
unityObjectGameObject _ (BuiltInClass c _) = builtInClassGameObject c
unityObjectGameObject _ (PrefabInstance _ _) = 0 -- Prefab instances don't reference GameObjects

source :: UnityObject -> ByteString
source (GameObject _ t) = t
source (Transform _ t) = t
source (MonoBehaviour _ t) = t
source (BuiltInClass _ t) = t
source (PrefabInstance _ t) = t

instance FromJSON GameObject where
  parseJSON = unwrapObject $ \go -> do
    n <- go .: "m_Name"
    return $ FullGameObject n

instance FromJSON Transform where
  parseJSON =
    (<|>)
      <$> parseTransform
      <*> parseRectTransform

parseTransform :: Value -> Parser Transform
parseTransform = unwrapObject $ \t -> do
  p <- t .:: "m_Father" >>= (.: "fileID")
  go <- t .:: "m_GameObject" >>= (.: "fileID")
  return $ FullTransform NormalTransform p go

parseRectTransform :: Value -> Parser Transform
parseRectTransform = unwrapObject $ \t -> do
  p <- t .:: "m_Father" >>= (.: "fileID")
  go <- t .:: "m_GameObject" >>= (.: "fileID")
  return $ FullTransform RectTransform p go

instance FromJSON MonoBehaviour where
  parseJSON = unwrapObject $ \mb -> do
    s <- mb .:: "m_Script" >>= (.: "guid")
    go <- mb .:: "m_GameObject" >>= (.: "fileID")
    return $ FullMonoBehaviour s go

instance FromJSON BuiltInClass where
  parseJSON = unwrapKeyAndObject $ \key inner -> do
    go <- (inner .:? "m_GameObject" .!= none) >>= withObject "GameObject" (.: "fileID")
    return $ FullBuiltInClass key go
    where
      none = Object (K.fromList [("fileID", Number 0)])

instance FromJSON PrefabInstance where
  parseJSON = unwrapKeyAndObject $ \key inner -> do
    c <- inner .:: "m_CorrespondingSourceObject" >>= (.: "guid")
    p <- inner .:: "m_PrefabInstance" >>= (.: "fileID")
    return $ FullPrefabInstance key c p

(.::) :: Object -> K.Key -> Parser Object
outer .:: key = do
  Object inner <- outer .: key
  return inner

unwrapObject :: (Object -> Parser a) -> Value -> Parser a
unwrapObject innerParser = unwrapKeyAndObject (const innerParser)

unwrapKeyAndObject :: (Text -> Object -> Parser a) -> Value -> Parser a
unwrapKeyAndObject innerParser = withObject "Outer" $ \outer -> do
  case K.toList outer of
    [(key, Object innerObj)] -> innerParser (toText key) innerObj
    _ -> fail "Expected exactly one top-level value of type Object"
