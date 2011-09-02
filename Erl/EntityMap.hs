module Erl.EntityMap (EntityMap,
                      empty,
                      singleton,
                      lookup,
                      member,
                      contains,
                      ids,
                      values,
                      toList,
                      fromList,
                      union,
                      difference,
                      intersection,
                      fold
                     ) where

import Control.Arrow (first)
import qualified Data.IntMap as DIM
import Prelude hiding (lookup)

import Erl.Entity (EntityId, toInt, fromInt)

newtype EntityMap a = EntityMap (DIM.IntMap a)

empty :: EntityMap a
empty = fromIntMap DIM.empty

singleton :: EntityId -> a -> EntityMap a
singleton id v = fromIntMap $ DIM.singleton (toInt id) v

lookup :: EntityId -> EntityMap a -> Maybe a
lookup id m = DIM.lookup (toInt id) (toIntMap m)

member :: EntityId -> EntityMap a -> Bool
member id m = DIM.member (toInt id) (toIntMap m)

contains :: EntityMap a -> EntityId -> Bool
contains = flip member

ids :: EntityMap a -> [EntityId]
ids = map fromInt . DIM.keys . toIntMap

values :: EntityMap a -> [a]
values = DIM.elems . toIntMap

toList :: EntityMap a -> [(EntityId, a)]
toList = map (first fromInt) . DIM.toList . toIntMap

fromList :: [(EntityId, a)] -> EntityMap a
fromList = fromIntMap . DIM.fromList . map (first toInt)

toIntMap :: EntityMap a -> DIM.IntMap a
toIntMap (EntityMap m) = m

fromIntMap :: DIM.IntMap a -> EntityMap a
fromIntMap = EntityMap

union :: EntityMap a -> EntityMap a -> EntityMap a
union = lift2 DIM.union

difference :: EntityMap a -> EntityMap a -> EntityMap a
difference = lift2 DIM.difference

intersection :: EntityMap a -> EntityMap a -> EntityMap a
intersection = lift2 DIM.intersection

fold :: (a -> b -> b) -> b -> EntityMap a -> b
fold f i = DIM.fold f i . toIntMap

lift2 :: (DIM.IntMap a -> DIM.IntMap a -> DIM.IntMap a) -> EntityMap a -> EntityMap a -> EntityMap a
lift2 f em1 em2 = fromIntMap $ f (toIntMap em1) (toIntMap em2)
