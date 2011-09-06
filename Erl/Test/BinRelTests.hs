module Erl.Test.BinRelTests where

import Control.Monad (liftM)
import qualified Data.Set as DS
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Erl.Entity (EntityId)
import qualified Erl.EntitySet as ES
import Erl.BinRel

import Erl.Test.EntityTests ()

runTests :: IO Bool
runTests =
  all isSuccess `liftM`
  sequence [quickCheckResult prop_sizeToList,
            quickCheckResult prop_fromListToList,
            quickCheckResult prop_empty,
            quickCheckResult prop_singleton,
            quickCheckResult prop_insert,
            quickCheckResult prop_delete,
            quickCheckResult prop_leftSet,
            quickCheckResult prop_rightSet,
            quickCheckResult $ forAll withMaybeMember prop_rightImage,
            quickCheckResult $ forAll withMaybeMember prop_leftImage,
            quickCheckResult $ forAll withMaybeTwoMembers prop_rightImage1,
            quickCheckResult $ forAll withMaybeTwoMembers prop_leftImage1]


prop_sizeToList :: BinRel -> Bool
prop_sizeToList r = size r == length (toList r)

prop_fromListToList :: [(EntityId, EntityId)] -> Bool
prop_fromListToList pairs =
  DS.fromList pairs == DS.fromList (toList $ fromList pairs)

prop_empty :: Bool
prop_empty = size empty == 0 && toList empty == []

prop_singleton :: (EntityId, EntityId) -> Bool
prop_singleton (x,y) =
  size r == 1 && [(x,y)] == toList r
    where r = singleton x y

prop_insert :: (BinRel, EntityId, EntityId) -> Bool
prop_insert (r, x, y) =
  elem (x,y) (toList r') &&
  member x y r' &&
  size r' == if member x y r then size r else size r + 1
    where r' = insert x y r

prop_delete :: (BinRel, EntityId, EntityId) -> Bool
prop_delete (r, x, y) =
  (not $ elem (x,y) $ toList r') &&
  not (member x y r') &&
  size r' == if member x y r then size r - 1 else size r
  where r' = delete x y r

prop_leftSet :: BinRel -> Bool
prop_leftSet r = leftSet r == ES.fromList (map fst $ toList r)

prop_rightSet :: BinRel -> Bool
prop_rightSet r = rightSet r == ES.fromList (map snd $ toList r)

prop_rightImage :: (BinRel, (EntityId, EntityId)) -> Bool
prop_rightImage (r, (x, y)) =
  member x y r == ES.member y (rightImage (ES.singleton x) r)

prop_leftImage :: (BinRel, (EntityId, EntityId)) -> Bool
prop_leftImage (r, (x, y)) =
  member x y r == ES.member x (leftImage (ES.singleton y) r)

prop_rightImage1 :: (BinRel, (EntityId, EntityId), (EntityId, EntityId)) -> Bool
prop_rightImage1 (r, (x, y), (w, z)) =
  rightImage (ES.fromList [x,w]) r == rightImage (ES.singleton x) r `ES.union` rightImage (ES.singleton w) r

prop_leftImage1 ::  (BinRel, (EntityId, EntityId), (EntityId, EntityId)) -> Bool
prop_leftImage1 (r, (x, y), (w, z)) =
  leftImage (ES.fromList [y,z]) r == leftImage (ES.singleton y) r `ES.union` leftImage (ES.singleton z) r

withMaybeMember :: Gen (BinRel, (EntityId, EntityId))
withMaybeMember = do
  r <- arbitrary
  p <- maybeMembers r
  return (r, p)

withMaybeTwoMembers :: Gen (BinRel, (EntityId, EntityId), (EntityId, EntityId))
withMaybeTwoMembers = do
  r <- arbitrary
  let pg = maybeMembers r
  p1 <- pg
  p2 <- pg
  return (r, p1, p2)

maybeMembers :: BinRel -> Gen (EntityId, EntityId)
maybeMembers r =
  if isEmpty r then arbitrary else frequency [(1, arbitrary), (2, elements (toList r))]

instance Arbitrary BinRel where
  arbitrary = fromList `liftM` arbitrary