{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.Entity where

newtype IdNum = IdNum Int deriving (Eq, Ord, Enum, Show)

data EntityId = EntityId { idNum :: !IdNum } deriving (Eq, Ord, Show)

data Entity d = Entity { id :: EntityId, attributes :: d } deriving (Eq, Ord, Show)
