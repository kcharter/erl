{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.Entity where

newtype EntityId = EntityId Int deriving (Eq, Ord, Enum, Show)

toInt :: EntityId -> Int
toInt (EntityId i) = i

fromInt :: Int -> EntityId
fromInt = EntityId
