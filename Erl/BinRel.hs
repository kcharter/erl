{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Erl.BinRel (BinRelId,
                   BinRel,
                   empty,
                   singleton,
                   insert,
                   delete,
                   size,
                   isEmpty,
                   member,
                   fromList,
                   toList,
                   leftSet,
                   rightSet,
                   rightImage,
                   leftImage,
                   rightImage',
                   leftImage',
                   compose,
                   union,
                   difference,
                   intersection) where

import Erl.Entity (EntityId)
import Erl.EntitySet (EntitySet)
import qualified Erl.EntitySet as ES
import Erl.EntityMap (EntityMap)
import qualified Erl.EntityMap as EM

newtype BinRelId = BinRelId Int deriving (Eq, Ord, Enum, Show)

data BinRel = BinRel { fromLeft :: EntityMap EntitySet,
                       fromRight :: EntityMap EntitySet } deriving (Eq, Ord, Show)

data Pair = Pair { leftId :: !EntityId, rightId :: !EntityId } deriving (Eq, Ord, Show)

empty :: BinRel
empty = BinRel { fromLeft = EM.empty, fromRight = EM.empty }

singleton :: EntityId -> EntityId -> BinRel
singleton x y = insert x y empty

insert :: EntityId -> EntityId -> BinRel -> BinRel
insert x y r = BinRel { fromLeft = fl', fromRight = fr' }
  where fl' = EM.alter (addTo y) x (fromLeft r)
        fr' = EM.alter (addTo x) y (fromRight r)
        addTo eid existing = Just $ maybe (ES.singleton eid) (ES.insert eid) existing

delete :: EntityId -> EntityId -> BinRel -> BinRel
delete x y r = BinRel { fromLeft = fl', fromRight = fr' }
  where fl' = if ES.isEmpty xImage' then EM.delete x fl else EM.insert x xImage' fl
        fl  = fromLeft r
        fr' = if ES.isEmpty yImage' then EM.delete y fr else EM.insert y yImage' fr
        fr  = fromRight r
        xImage' = ES.delete y xImage
        yImage' = ES.delete x yImage
        xImage = rightImage' x r
        yImage = leftImage'  y r

size :: BinRel -> Int
size = EM.fold addSize 0 . fromLeft
  where addSize s = (ES.size s +)

isEmpty :: BinRel -> Bool
isEmpty = (0 ==) . size

member :: EntityId -> EntityId -> BinRel -> Bool
member x y r = maybe False (ES.member y) $ EM.lookup x (fromLeft r)

fromList :: [(EntityId, EntityId)] -> BinRel
fromList = foldr (uncurry insert) empty

toList :: BinRel -> [(EntityId, EntityId)]
toList = concatMap mkPairs . EM.toList . fromLeft
  where mkPairs (eid, image) = zip (repeat eid) (ES.toList image)

leftSet :: BinRel -> EntitySet
leftSet = ES.fromList . EM.ids . fromLeft

rightSet :: BinRel -> EntitySet
rightSet = ES.fromList . EM.ids . fromRight

rightImage :: EntitySet -> BinRel -> EntitySet
rightImage = image (flip rightImage')

leftImage :: EntitySet -> BinRel -> EntitySet
leftImage = image (flip leftImage')

image :: (BinRel -> EntityId -> EntitySet) -> EntitySet -> BinRel -> EntitySet
image imageOfOne es r = foldr ES.union ES.empty $ map (imageOfOne r) (ES.toList es)

rightImage' :: EntityId -> BinRel -> EntitySet
rightImage' eid = maybe ES.empty id . EM.lookup eid . fromLeft

leftImage' :: EntityId -> BinRel -> EntitySet
leftImage' eid = maybe ES.empty id . EM.lookup eid . fromRight

image' :: (BinRel -> EntityMap EntitySet) -> EntityId -> BinRel -> EntitySet
image' toImageMap eid = maybe ES.empty id . EM.lookup eid . toImageMap

compose :: BinRel -> BinRel -> BinRel
compose r q = BinRel { fromLeft = fl', fromRight = fr' }
  where fl' = EM.mapMaybe furtherRight $ fromLeft  r
        fr' = EM.mapMaybe furtherLeft  $ fromRight q
        furtherRight s = nothingIfEmpty $ rightImage s q
        furtherLeft  s = nothingIfEmpty $ leftImage  s r

union :: BinRel -> BinRel -> BinRel
union r q = BinRel { fromLeft = fl', fromRight = fr' }
  where fl' = EM.unionWith ES.union (fromLeft r) (fromLeft q)
        fr' = EM.unionWith ES.union (fromRight r) (fromRight q)

difference :: BinRel -> BinRel -> BinRel
difference r q = BinRel { fromLeft = fl', fromRight = fr' }
  where fl' = EM.differenceWith nonEmptyDiff (fromLeft r) (fromLeft q)
        fr' = EM.differenceWith nonEmptyDiff (fromRight r) (fromLeft q)
        nonEmptyDiff s t = nothingIfEmpty $ ES.difference s t

intersection :: BinRel -> BinRel -> BinRel
intersection r q = BinRel { fromLeft = fl', fromRight = fr' }
  where fl' = intersectImageMaps (fromLeft r) (fromLeft q)
        fr' = intersectImageMaps (fromRight r) (fromRight q)
        intersectImageMaps m n = discardEmpties $ EM.intersectionWith ES.intersection m n
        discardEmpties = EM.filter (not . ES.isEmpty)

nothingIfEmpty :: EntitySet -> Maybe EntitySet
nothingIfEmpty s = if ES.isEmpty s then Nothing else Just s

ni = error "not implemented"
