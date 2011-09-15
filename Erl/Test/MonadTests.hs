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
            quickCheckResult prop_deleteEntity,
            quickCheckResult prop_addEntity,
            quickCheckResult prop_removeEntity,
            quickCheckResult prop_createBinRel,
            quickCheckResult prop_deleteBinRel]

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
    checkNoneOf [haveEntitySet esid,
                 inEntitySetIds esid]

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
    checkNoneOf [hasEntity eid,
                 elem eid `liftM` entityIds]

prop_addEntity :: (ErlState Int, EntitySetId, EntityId, Int) -> Bool
prop_addEntity (s, esid, eid, val) =
  checkErl s $
  caseHasSet esid haveSetCase noSetCase
    where haveSetCase =
            caseHasEntity eid happyCase noEntityCase
          noSetCase =
            failsWithNoSuchEntitySet esid addition
          happyCase =
            addition >>
            (maybe False (val ==) `liftM` lookupEntity eid esid)
          noEntityCase =
            failsWithNoSuchEntity eid addition
          addition = addEntity eid val esid

prop_removeEntity :: (ErlState Int, EntitySetId, EntityId) -> Bool
prop_removeEntity (s, esid, eid) =
  checkErl s $
  caseHasSet esid haveSetCase noSetCase
    where haveSetCase = caseHasEntity eid happyCase noEntityCase
          noSetCase =
            failsWithNoSuchEntitySet esid removal
          happyCase =
            removal >>
            (maybe True (const False) `liftM` lookupEntity eid esid)
          noEntityCase =
            failsWithNoSuchEntity eid removal
          removal = removeEntity eid esid

prop_createBinRel :: ErlState Int -> Bool
prop_createBinRel s =
  checkErl s $ do
    bid <- createBinRel
    checkAll [haveBinRel bid,
              inBinRelIds bid]

prop_deleteBinRel :: ErlState Int -> Bool
prop_deleteBinRel s =
  checkErl s $ do
    bid <- createBinRel
    deleteBinRel bid
    checkNoneOf [haveBinRel bid,
                 inBinRelIds bid]

haveBinRel :: (MonadErl d m) => BinRelId -> m Bool
haveBinRel bid =
  maybe False (const True) `liftM` lookupBinRel bid

inBinRelIds :: (MonadErl d m) => BinRelId -> m Bool
inBinRelIds bid = elem bid `liftM` binRelIds

caseHasSet :: (MonadErl d m) => EntitySetId -> m a -> m a -> m a
caseHasSet esid yesCase noCase =
  maybe noCase (const yesCase) =<< lookupEntitySet esid

caseHasEntity :: (MonadErl d m) => EntityId -> m a -> m a -> m a
caseHasEntity eid yesCase noCase =
  hasEntity eid >>=
  (\present -> if present then yesCase else noCase)

checkErl :: ErlState d -> Erl d Bool -> Bool
checkErl s erl =
  either (const False) id $ evalErl erl s

checkAll :: (Monad m) => [m Bool] -> m Bool
checkAll = (and `liftM`) . sequence

checkNoneOf :: (Monad m) => [m Bool] -> m Bool
checkNoneOf = checkAll . map (liftM not)

failsWithSomeError :: (MonadErl d m) => m a -> m Bool
failsWithSomeError m =
  (m >> return False) `catchError` (const $ return True)

failsWithNoSuchEntitySet :: (MonadErl d m) => EntitySetId -> m a -> m Bool
failsWithNoSuchEntitySet esid = failsWithError (NoSuchEntitySet esid)

failsWithNoSuchEntity :: (MonadErl d m) => EntityId -> m a -> m Bool
failsWithNoSuchEntity eid = failsWithError (NoSuchEntity eid)

failsWithError :: (MonadErl d m) => ErlError -> m a -> m Bool
failsWithError expectedError op =
  (op >> return False) `catchError` (return . (expectedError ==))

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
