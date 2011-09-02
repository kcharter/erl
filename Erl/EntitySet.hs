{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.EntitySet (EntitySet,
                      empty,
                      singleton,
                      add,
                      remove,
                      member,
                      contains,
                      isEmpty,
                      size,
                      union,
                      difference,
                      intersection,
                      toList,
                      fromList) where

import qualified Data.IntSet as DIS

import Erl.Entity

newtype EntitySet = EntitySet DIS.IntSet deriving (Eq, Ord, Show)

empty :: EntitySet
empty = EntitySet DIS.empty

singleton :: EntityId -> EntitySet
singleton = fromIntSet . DIS.singleton . idToInt

add :: EntityId -> EntitySet -> EntitySet
add id es = fromIntSet $ DIS.insert (idToInt id) (toIntSet es)

remove :: EntityId -> EntitySet -> EntitySet
remove id es = fromIntSet $ DIS.delete (idToInt id) (toIntSet es)

member :: EntityId -> EntitySet -> Bool
member id es = DIS.member (idToInt id) (toIntSet es)

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
toList es = map intToId $ DIS.toList $ toIntSet es

fromList :: [EntityId] -> EntitySet
fromList = fromIntSet . DIS.fromList . map idToInt

idToInt :: EntityId -> Int
idToInt (EntityId i) = i

intToId :: Int -> EntityId
intToId = EntityId

toIntSet :: EntitySet -> DIS.IntSet
toIntSet (EntitySet s) = s

fromIntSet :: DIS.IntSet -> EntitySet
fromIntSet = EntitySet
