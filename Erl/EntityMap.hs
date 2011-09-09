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

import qualified Data.Map as DM
import Prelude hiding (lookup, map, filter)

import Erl.Entity (EntityId)

newtype EntityMap a = EntityMap (DM.Map EntityId a) deriving (Eq, Ord, Show)

empty :: EntityMap a
empty = fromMap DM.empty

singleton :: EntityId -> a -> EntityMap a
singleton id v = fromMap $ DM.singleton id v

lookup :: EntityId -> EntityMap a -> Maybe a
lookup id m = DM.lookup id (toMap m)

member :: EntityId -> EntityMap a -> Bool
member id m = DM.member id (toMap m)

contains :: EntityMap a -> EntityId -> Bool
contains = flip member

insert :: EntityId -> a -> EntityMap a -> EntityMap a
insert i v = lift1 $ DM.insert i v

delete :: EntityId -> EntityMap a -> EntityMap a
delete = lift1 . DM.delete

ids :: EntityMap a -> [EntityId]
ids = DM.keys . toMap

values :: EntityMap a -> [a]
values = DM.elems . toMap

toList :: EntityMap a -> [(EntityId, a)]
toList = DM.toList . toMap

fromList :: [(EntityId, a)] -> EntityMap a
fromList = fromMap . DM.fromList

toMap :: EntityMap a -> DM.Map EntityId a
toMap (EntityMap m) = m

fromMap :: DM.Map EntityId a -> EntityMap a
fromMap = EntityMap

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
fold f i = DM.fold f i . toMap

foldWithKey :: (EntityId -> a -> b -> b) -> b -> EntityMap a -> b
foldWithKey f i = DM.foldrWithKey f i . toMap

adjust :: (a -> a) -> EntityId -> EntityMap a -> EntityMap a
adjust f i = lift1 $ DM.adjust f i

alter :: (Maybe a -> Maybe a) -> EntityId -> EntityMap a -> EntityMap a
alter f i = lift1 $ DM.alter f i

map :: (a -> b) -> EntityMap a -> EntityMap b
map = lift1 . DM.map

mapMaybe :: (a -> Maybe b) -> EntityMap a -> EntityMap b
mapMaybe = lift1 . DM.mapMaybe

filter :: (a -> Bool) -> EntityMap a -> EntityMap a
filter = lift1 . DM.filter

filterWithKey :: (EntityId -> a -> Bool) -> EntityMap a -> EntityMap a
filterWithKey = lift1 . DM.filterWithKey

lift1 :: (DM.Map EntityId a -> DM.Map EntityId b) -> EntityMap a -> EntityMap b
lift1 f = fromMap . f . toMap

lift2 :: (DM.Map EntityId a -> DM.Map EntityId a -> DM.Map EntityId a) -> EntityMap a -> EntityMap a -> EntityMap a
lift2 f em1 em2 = fromMap $ f (toMap em1) (toMap em2)
