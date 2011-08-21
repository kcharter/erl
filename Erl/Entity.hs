module Erl.Entity where

import qualified Erl.EntityType as Type

newtype IdNum = IdNum Int deriving (Eq, Ord, Show)

data Id = Unscoped { typeId :: !Type.Id, idNum :: !IdNum } |
          Scoped   { typeId :: !Type.Id, idNum :: !IdNum, ownerId :: Id } deriving (Eq, Ord, Show)
