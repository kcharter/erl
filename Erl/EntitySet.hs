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

import qualified Data.IntSet as DIS

import Erl.Entity

newtype EntitySet = EntitySet DIS.IntSet deriving (Eq, Ord, Show)

empty :: EntitySet
empty = EntitySet DIS.empty

singleton :: EntityId -> EntitySet
singleton = fromIntSet . DIS.singleton . toInt

insert :: EntityId -> EntitySet -> EntitySet
insert id es = fromIntSet $ DIS.insert (toInt id) (toIntSet es)

delete :: EntityId -> EntitySet -> EntitySet
delete id es = fromIntSet $ DIS.delete (toInt id) (toIntSet es)

member :: EntityId -> EntitySet -> Bool
member id es = DIS.member (toInt id) (toIntSet es)

contains :: EntitySet -> EntityId -> Bool
contains = flip member

isEmpty :: EntitySet -> Bool
isEmpty = (0 ==) . size

size :: EntitySet -> Int
size = DIS.size . toIntSet

union :: EntitySet -> EntitySet -> EntitySet
union = lift2 DIS.union

difference :: EntitySet -> EntitySet -> EntitySet
difference = lift2 DIS.difference

intersection :: EntitySet -> EntitySet -> EntitySet
intersection = lift2 DIS.intersection

lift2 :: (DIS.IntSet -> DIS.IntSet -> DIS.IntSet) -> EntitySet -> EntitySet -> EntitySet
lift2 f es1 es2 = fromIntSet $ f (toIntSet es1) (toIntSet es2)

toList :: EntitySet -> [EntityId]
toList es = map fromInt $ DIS.toList $ toIntSet es

fromList :: [EntityId] -> EntitySet
fromList = fromIntSet . DIS.fromList . map toInt

isSubsetOf :: EntitySet -> EntitySet -> Bool
isSubsetOf es1 es2 = DIS.isSubsetOf (toIntSet es1) (toIntSet es2)

toIntSet :: EntitySet -> DIS.IntSet
toIntSet (EntitySet s) = s

fromIntSet :: DIS.IntSet -> EntitySet
fromIntSet = EntitySet
