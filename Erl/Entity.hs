module Erl.Entity where

import qualified Data.Text as T

import qualified Erl.EntityType as Type

newtype IdNum = IdNum Int deriving (Eq, Ord, Show)

data Id = Unscoped { typeId :: !Type.Id, idNum :: !IdNum } |
          Scoped   { typeId :: !Type.Id, idNum :: !IdNum, ownerId :: Id } deriving (Eq, Ord, Show)

newtype Name = Name T.Text deriving (Eq, Ord, Show)

data Instance d = Instance { id :: Id, name :: Name, attributes :: d }
