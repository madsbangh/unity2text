module Context (getObject, getName, getTransformId, Context (Context)) where

import Data.Map (Map, lookup)
import Data.Text (Text)
import Unity.AssetModel (FileId, UnityObject)
import Unity.Meta (Guid)
import Prelude hiding (lookup)

data Context = Context
  { objectsByFileId :: Map FileId UnityObject,
    assetNamesByGuid :: Map Guid Text,
    transformsByGameObject :: Map FileId FileId
  }

getObject :: FileId -> (Context -> Maybe UnityObject)
getObject fid = lookup fid . objectsByFileId

getName :: Guid -> (Context -> Maybe Text)
getName fid = lookup fid . assetNamesByGuid

getTransformId :: GameObjectFileId -> (Context -> Maybe FileId)
getTransformId fid = lookup fid . transformsByGameObject

type GameObjectFileId = FileId