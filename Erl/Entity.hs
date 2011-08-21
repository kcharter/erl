module Erl.Entity where

import Erl.Name
import qualified Erl.EntityType as Type

newtype IdNum = IdNum Int deriving (Eq, Ord, Show)

data Id = Unscoped { typeId :: !Type.Id, idNum :: !IdNum } |
          Scoped   { typeId :: !Type.Id, idNum :: !IdNum, ownerId :: Id } deriving (Eq, Ord, Show)

data Instance d = Instance { id :: Id, name :: Name, attributes :: d } deriving (Eq, Ord, Show)
