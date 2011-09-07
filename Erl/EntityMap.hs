module Erl.EntityMap (EntityMap,
                      empty,
                      singleton,
                      lookup,
                      member,
                      contains,
                      insert,
                      delete,
                      ids,
                      values,
                      toList,
                      fromList,
                      union,
                      unionWith,
                      difference,
                      differenceWith,
                      intersection,
                      intersectionWith,
                      fold,
                      foldWithKey,
                      adjust,
                      alter,
                      map,
                      mapMaybe,
                      filter,
                      filterWithKey
                     ) where

import Control.Arrow (first)
import qualified Data.IntMap as DIM
import Prelude hiding (lookup, map, filter)
import qualified Prelude as P

import Erl.Entity (EntityId, toInt, fromInt)

newtype EntityMap a = EntityMap (DIM.IntMap a) deriving (Eq, Ord, Show)

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

insert :: EntityId -> a -> EntityMap a -> EntityMap a
insert i v = lift1 $ DIM.insert (toInt i) v

delete :: EntityId -> EntityMap a -> EntityMap a
delete = lift1 . DIM.delete . toInt

ids :: EntityMap a -> [EntityId]
ids = P.map fromInt . DIM.keys . toIntMap

values :: EntityMap a -> [a]
values = DIM.elems . toIntMap

toList :: EntityMap a -> [(EntityId, a)]
toList = P.map (first fromInt) . DIM.toList . toIntMap

fromList :: [(EntityId, a)] -> EntityMap a
fromList = fromIntMap . DIM.fromList . P.map (first toInt)

toIntMap :: EntityMap a -> DIM.IntMap a
toIntMap (EntityMap m) = m

fromIntMap :: DIM.IntMap a -> EntityMap a
fromIntMap = EntityMap

union :: EntityMap a -> EntityMap a -> EntityMap a
union = lift2 DIM.union

unionWith :: (a -> a -> a) -> EntityMap a -> EntityMap a -> EntityMap a
unionWith f = lift2 (DIM.unionWith f)

difference :: EntityMap a -> EntityMap a -> EntityMap a
difference = lift2 DIM.difference

differenceWith :: (a -> a -> Maybe a) -> EntityMap a -> EntityMap a -> EntityMap a
differenceWith f = lift2 (DIM.differenceWith f)

intersection :: EntityMap a -> EntityMap a -> EntityMap a
intersection = lift2 DIM.intersection

intersectionWith :: (a -> a -> a) -> EntityMap a -> EntityMap a -> EntityMap a
intersectionWith f = lift2 (DIM.intersectionWith f)

fold :: (a -> b -> b) -> b -> EntityMap a -> b
fold f i = DIM.fold f i . toIntMap

foldWithKey :: (EntityId -> a -> b -> b) -> b -> EntityMap a -> b
foldWithKey f i = DIM.foldWithKey f' i . toIntMap
  where f' raw = f (fromInt raw)

adjust :: (a -> a) -> EntityId -> EntityMap a -> EntityMap a
adjust f i = lift1 $ DIM.adjust f (toInt i)

alter :: (Maybe a -> Maybe a) -> EntityId -> EntityMap a -> EntityMap a
alter f i = lift1 $ DIM.alter f (toInt i)

map :: (a -> b) -> EntityMap a -> EntityMap b
map = lift1 . DIM.map

mapMaybe :: (a -> Maybe b) -> EntityMap a -> EntityMap b
mapMaybe = lift1 . DIM.mapMaybe

filter :: (a -> Bool) -> EntityMap a -> EntityMap a
filter = lift1 . DIM.filter

filterWithKey :: (EntityId -> a -> Bool) -> EntityMap a -> EntityMap a
filterWithKey pred = lift1 (DIM.filterWithKey pred')
  where pred' raw = pred (fromInt raw)

lift1 :: (DIM.IntMap a -> DIM.IntMap b) -> EntityMap a -> EntityMap b
lift1 f = fromIntMap . f . toIntMap

lift2 :: (DIM.IntMap a -> DIM.IntMap a -> DIM.IntMap a) -> EntityMap a -> EntityMap a -> EntityMap a
lift2 f em1 em2 = fromIntMap $ f (toIntMap em1) (toIntMap em2)
