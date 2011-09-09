{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.EntitySet (EntitySet,
                      empty,
                      singleton,
                      insert,
                      delete,
                      member,
                      contains,
                      isEmpty,
                      size,
                      union,
                      difference,
                      intersection,
                      toList,
                      fromList,
                      isSubsetOf) where

import qualified Data.Set as DS

import Erl.Entity

newtype EntitySet = EntitySet (DS.Set EntityId) deriving (Eq, Ord, Show)

empty :: EntitySet
empty = EntitySet DS.empty

singleton :: EntityId -> EntitySet
singleton = fromSet . DS.singleton

insert :: EntityId -> EntitySet -> EntitySet
insert id es = fromSet $ DS.insert id (toSet es)

delete :: EntityId -> EntitySet -> EntitySet
delete id es = fromSet $ DS.delete id (toSet es)

member :: EntityId -> EntitySet -> Bool
member id es = DS.member id (toSet es)

contains :: EntitySet -> EntityId -> Bool
contains = flip member

isEmpty :: EntitySet -> Bool
isEmpty = (0 ==) . size

size :: EntitySet -> Int
size = DS.size . toSet

union :: EntitySet -> EntitySet -> EntitySet
union = lift2 DS.union

difference :: EntitySet -> EntitySet -> EntitySet
difference = lift2 DS.difference

intersection :: EntitySet -> EntitySet -> EntitySet
intersection = lift2 DS.intersection

lift2 :: (DS.Set EntityId -> DS.Set EntityId -> DS.Set EntityId) -> EntitySet -> EntitySet -> EntitySet
lift2 f es1 es2 = fromSet $ f (toSet es1) (toSet es2)

toList :: EntitySet -> [EntityId]
toList es = DS.toList $ toSet es

fromList :: [EntityId] -> EntitySet
fromList = fromSet . DS.fromList

isSubsetOf :: EntitySet -> EntitySet -> Bool
isSubsetOf es1 es2 = DS.isSubsetOf (toSet es1) (toSet es2)

toSet :: EntitySet -> DS.Set EntityId
toSet (EntitySet s) = s

fromSet :: DS.Set EntityId -> EntitySet
fromSet = EntitySet
