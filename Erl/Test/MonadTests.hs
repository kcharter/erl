module Erl.Test.MonadTests where

import Control.Monad (liftM)
import Control.Monad.Error (catchError)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import Erl.Entity (EntityId)
import Erl.Monad

import Erl.Test.EntityTests ()

runTests :: IO Bool
runTests =
  all isSuccess `liftM`
  sequence [quickCheckResult prop_createEntitySet,
            quickCheckResult prop_deleteEntitySet,
            quickCheckResult prop_createEntity,
            quickCheckResult prop_deleteEntity]

prop_createEntitySet :: ErlState Int -> Bool
prop_createEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    checkAll [haveEntitySet esid,
              inEntitySetIds esid]

prop_deleteEntitySet :: ErlState Int -> Bool
prop_deleteEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    deleteEntitySet esid
    checkAll [not `liftM` haveEntitySet esid,
              not `liftM` inEntitySetIds esid]

haveEntitySet :: (MonadErl d m) => EntitySetId -> m Bool
haveEntitySet esid =
  maybe False (const True) `liftM` lookupEntitySet esid

inEntitySetIds :: (MonadErl d m) => EntitySetId -> m Bool
inEntitySetIds esid = elem esid `liftM` entitySetIds

prop_createEntity :: ErlState Int -> Bool
prop_createEntity s =
  checkErl s $ do
    eid <- createEntity
    checkAll [hasEntity eid,
              elem eid `liftM` entityIds]

prop_deleteEntity :: (ErlState Int, EntityId) -> Bool
prop_deleteEntity (s, eid) = do
  checkErl s $ do
    deleteEntity eid
    checkAll [not `liftM` hasEntity eid,
              (not . elem eid) `liftM` entityIds]

checkErl :: ErlState d -> Erl d Bool -> Bool
checkErl s erl =
  either (const False) id $ evalErl erl s

checkAll :: (Monad m) => [m Bool] -> m Bool
checkAll = (and `liftM`) . sequence

checkFails :: (MonadErl d m) => m a -> m Bool
checkFails m =
  (m >> return False) `catchError` (const $ return True)

instance (Arbitrary d) => Arbitrary (ErlState d) where
  arbitrary = do
    s1 <- foldr addEntitySet emptyState `liftM` arbitrary
    -- TODO: add some entities to some entity sets with data
    foldr addEntity s1 `liftM` arbitrary
      where addEntitySet () = execErl createEntitySet
            addEntity () = execErl createEntity

withEntitySetIdAndVal :: (Arbitrary d) => Gen (ErlState d, EntitySetId, d)
withEntitySetIdAndVal = do
  s     <- arbitrary
  esids <- sampleEntitySetIds s
  esid  <- if null esids then arbitrary else elements esids
  val   <- arbitrary
  return (s, esid, val)

sampleEntitySetIds :: (ErlState d) -> Gen [EntitySetId]
sampleEntitySetIds s =
  if null esids then return [] else listOf (elements esids)
    where esids = either (error . show) id $ evalErl entitySetIds s

withEntity :: (Arbitrary d) => Gen (ErlState d, EntityId)
withEntity = do
  s <- arbitrary
  let ids = either (const []) id $ evalErl entityIds s
  id <- if null ids then arbitrary else elements ids
  return (s, id)

instance Arbitrary EntitySetId where
  arbitrary = entitySetId `liftM` arbitrary
