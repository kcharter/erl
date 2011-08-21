module Erl.EntityType where

import Erl.Name

newtype Id = Id Int deriving (Eq, Ord, Show)

data EntityType = EntityType { id :: Id, name :: Name } deriving (Eq, Ord, Show)
