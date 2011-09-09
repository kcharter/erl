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
import qualified Data.Map as DM
import Prelude hiding (lookup, map, filter)
import qualified Prelude as P

import Erl.Entity (EntityId, toInt, fromInt)

newtype EntityMap a = EntityMap (DM.Map Int a) deriving (Eq, Ord, Show)

empty :: EntityMap a
empty = fromIntMap DM.empty

singleton :: EntityId -> a -> EntityMap a
singleton id v = fromIntMap $ DM.singleton (toInt id) v

lookup :: EntityId -> EntityMap a -> Maybe a
lookup id m = DM.lookup (toInt id) (toIntMap m)

member :: EntityId -> EntityMap a -> Bool
member id m = DM.member (toInt id) (toIntMap m)

contains :: EntityMap a -> EntityId -> Bool
contains = flip member

insert :: EntityId -> a -> EntityMap a -> EntityMap a
insert i v = lift1 $ DM.insert (toInt i) v

delete :: EntityId -> EntityMap a -> EntityMap a
delete = lift1 . DM.delete . toInt

ids :: EntityMap a -> [EntityId]
ids = P.map fromInt . DM.keys . toIntMap

values :: EntityMap a -> [a]
values = DM.elems . toIntMap

toList :: EntityMap a -> [(EntityId, a)]
toList = P.map (first fromInt) . DM.toList . toIntMap

fromList :: [(EntityId, a)] -> EntityMap a
fromList = fromIntMap . DM.fromList . P.map (first toInt)

toIntMap :: EntityMap a -> DM.Map Int a
toIntMap (EntityMap m) = m

fromIntMap :: DM.Map Int a -> EntityMap a
fromIntMap = EntityMap

union :: EntityMap a -> EntityMap a -> EntityMap a
union = lift2 DM.union

unionWith :: (a -> a -> a) -> EntityMap a -> EntityMap a -> EntityMap a
unionWith f = lift2 (DM.unionWith f)

difference :: EntityMap a -> EntityMap a -> EntityMap a
difference = lift2 DM.difference

differenceWith :: (a -> a -> Maybe a) -> EntityMap a -> EntityMap a -> EntityMap a
differenceWith f = lift2 (DM.differenceWith f)

intersection :: EntityMap a -> EntityMap a -> EntityMap a
intersection = lift2 DM.intersection

intersectionWith :: (a -> a -> a) -> EntityMap a -> EntityMap a -> EntityMap a
intersectionWith f = lift2 (DM.intersectionWith f)

fold :: (a -> b -> b) -> b -> EntityMap a -> b
fold f i = DM.fold f i . toIntMap

foldWithKey :: (EntityId -> a -> b -> b) -> b -> EntityMap a -> b
foldWithKey f i = DM.foldWithKey f' i . toIntMap
  where f' raw = f (fromInt raw)

adjust :: (a -> a) -> EntityId -> EntityMap a -> EntityMap a
adjust f i = lift1 $ DM.adjust f (toInt i)

alter :: (Maybe a -> Maybe a) -> EntityId -> EntityMap a -> EntityMap a
alter f i = lift1 $ DM.alter f (toInt i)

map :: (a -> b) -> EntityMap a -> EntityMap b
map = lift1 . DM.map

mapMaybe :: (a -> Maybe b) -> EntityMap a -> EntityMap b
mapMaybe = lift1 . DM.mapMaybe

filter :: (a -> Bool) -> EntityMap a -> EntityMap a
filter = lift1 . DM.filter

filterWithKey :: (EntityId -> a -> Bool) -> EntityMap a -> EntityMap a
filterWithKey pred = lift1 (DM.filterWithKey pred')
  where pred' raw = pred (fromInt raw)

lift1 :: (DM.Map Int a -> DM.Map Int b) -> EntityMap a -> EntityMap b
lift1 f = fromIntMap . f . toIntMap

lift2 :: (DM.Map Int a -> DM.Map Int a -> DM.Map Int a) -> EntityMap a -> EntityMap a -> EntityMap a
lift2 f em1 em2 = fromIntMap $ f (toIntMap em1) (toIntMap em2)
