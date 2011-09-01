{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.Entity where

import qualified Erl.EntityType as Type

newtype IdNum = IdNum Int deriving (Eq, Ord, Enum, Show)

data Id = Unscoped { typeId :: !Type.Id, idNum :: !IdNum } deriving (Eq, Ord, Show)

data Instance d = Instance { id :: Id, attributes :: d } deriving (Eq, Ord, Show)
