module ObjectGraph where

import Context
import qualified Data.Map as M
import Unity.AssetModel

lookupParentGameObject :: Context -> FileId -> Maybe FileId
lookupParentGameObject context child = do
  t <- lookupGameObjectTransform context child
  p <- lookupTransformParent context t
  return $ transformGameObject p

lookupTransformParent :: Context -> Transform -> Maybe Transform
lookupTransformParent context child = do
  let parentId = parent child
  Transform t _ <- getObject parentId context
  return t

isTransform :: UnityObject -> Bool
isTransform (Transform {}) = True
isTransform _ = False

lookupGameObjectTransform :: Context -> FileId -> Maybe Transform
lookupGameObjectTransform context gameObjectFileId = do
  transformFileId <- getTransformId gameObjectFileId context
  Transform t _ <- getObject transformFileId context
  return t

createGameObjectTransformLookup :: [(FileId, UnityObject)] -> M.Map FileId FileId
createGameObjectTransformLookup asset = M.fromList mappings
  where
    mappings = do
      (tfid, Transform t _) <- asset
      return (transformGameObject t, tfid)
