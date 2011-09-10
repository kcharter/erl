{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.Entity where

newtype EntityId = EntityId Int deriving (Eq, Ord, Enum, Show)

fromInt :: Int -> EntityId
fromInt = EntityId
