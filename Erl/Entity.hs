{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.Entity where

newtype EntityId = EntityId Int deriving (Eq, Ord, Enum, Show)

data Entity d = Entity { id :: EntityId, attributes :: d } deriving (Eq, Ord, Show)

toInt :: EntityId -> Int
toInt (EntityId i) = i

fromInt :: Int -> EntityId
fromInt = EntityId
