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
  sequence [quickCheckResult prop_createEntity,
            quickCheckResult prop_deleteEntity,
            quickCheckResult prop_createEntitySet,
            quickCheckResult prop_deleteEntitySet,
            quickCheckResult prop_addEntity,
            quickCheckResult prop_removeEntity,
            quickCheckResult prop_createBinRel,
            quickCheckResult prop_deleteBinRel,
            quickCheckResult prop_addPair,
            quickCheckResult prop_removePair]

prop_createEntity :: ErlState Int -> Bool
prop_createEntity s =
  checkErl s $ do
    eid <- createEntity
    allOf [hasEntity eid,
           elem eid `liftM` entityIds]

prop_deleteEntity :: (ErlState Int, EntityId) -> Bool
prop_deleteEntity (s, eid) = do
  checkErl s $ do
    deleteEntity eid
    noneOf [hasEntity eid,
            elem eid `liftM` entityIds]

prop_createEntitySet :: ErlState Int -> Bool
prop_createEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    allOf [haveEntitySet esid,
           inEntitySetIds esid]

prop_deleteEntitySet :: ErlState Int -> Bool
prop_deleteEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    deleteEntitySet esid
    noneOf [haveEntitySet esid,
            inEntitySetIds esid]

haveEntitySet :: (MonadErl d m) => EntitySetId -> m Bool
haveEntitySet esid =
  maybe False (const True) `liftM` lookupEntitySet esid

inEntitySetIds :: (MonadErl d m) => EntitySetId -> m Bool
inEntitySetIds esid = elem esid `liftM` entitySetIds

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
    allOf [haveBinRel bid,
           inBinRelIds bid]

prop_deleteBinRel :: ErlState Int -> Bool
prop_deleteBinRel s =
  checkErl s $ do
    bid <- createBinRel
    deleteBinRel bid
    noneOf [haveBinRel bid,
            inBinRelIds bid]

haveBinRel :: (MonadErl d m) => BinRelId -> m Bool
haveBinRel bid =
  maybe False (const True) `liftM` lookupBinRel bid

inBinRelIds :: (MonadErl d m) => BinRelId -> m Bool
inBinRelIds bid = elem bid `liftM` binRelIds

prop_addPair :: (ErlState Int, BinRelId, EntityId, EntityId, Int) -> Bool
prop_addPair (s, bid, eid1, eid2, val) =
  checkErl s $
  caseHasBinRel bid haveRelCase noRelCase
    where haveRelCase =
            caseHasEntity eid1 haveEntity1 noEntity1
          noRelCase =
            failsWithNoSuchBinRel bid addition
          haveEntity1 =
            caseHasEntity eid2 happyCase noEntity2
          happyCase =
            addition >>
            maybe False (val ==) `liftM` lookupPair eid1 eid2 bid
          noEntity1 =
            failsWithNoSuchEntity eid1 addition
          noEntity2 =
            failsWithNoSuchEntity eid2 addition
          addition = addPair eid1 eid2 val bid

prop_removePair :: (ErlState Int, EntityId, EntityId, BinRelId) -> Bool
prop_removePair (s, eid1, eid2, bid) =
  checkErl s $
  caseHasBinRel bid haveRelCase noRelCase
    where haveRelCase =
            caseHasEntity eid1 haveEntity1 noEntity1
          noRelCase =
            failsWithNoSuchBinRel bid removal
          haveEntity1 =
            caseHasEntity eid2 happyCase noEntity2
          happyCase =
            removal >>
            maybe True (const False) `liftM` lookupPair eid1 eid2 bid
          noEntity1 =
            failsWithNoSuchEntity eid1 removal
          noEntity2 =
            failsWithNoSuchEntity eid2 removal
          removal = removePair eid1 eid2 bid

caseHasSet :: (MonadErl d m) => EntitySetId -> m a -> m a -> m a
caseHasSet esid yesCase noCase =
  maybe noCase (const yesCase) =<< lookupEntitySet esid

caseHasEntity :: (MonadErl d m) => EntityId -> m a -> m a -> m a
caseHasEntity eid yesCase noCase =
  hasEntity eid >>=
  (\present -> if present then yesCase else noCase)

caseHasBinRel :: (MonadErl d m) => BinRelId -> m a -> m a -> m a
caseHasBinRel bid yesCase noCase =
  maybe noCase (const yesCase) =<< lookupBinRel bid

checkErl :: ErlState d -> Erl d Bool -> Bool
checkErl s erl =
  either (const False) id $ evalErl erl s

allOf :: (Monad m) => [m Bool] -> m Bool
allOf = (and `liftM`) . sequence

noneOf :: (Monad m) => [m Bool] -> m Bool
noneOf = allOf . map (liftM not)

failsWithSomeError :: (MonadErl d m) => m a -> m Bool
failsWithSomeError m =
  (m >> return False) `catchError` (const $ return True)

failsWithNoSuchEntitySet :: (MonadErl d m) => EntitySetId -> m a -> m Bool
failsWithNoSuchEntitySet esid = failsWithError (NoSuchEntitySet esid)

failsWithNoSuchEntity :: (MonadErl d m) => EntityId -> m a -> m Bool
failsWithNoSuchEntity eid = failsWithError (NoSuchEntity eid)

failsWithNoSuchBinRel :: (MonadErl d m) => BinRelId -> m a -> m Bool
failsWithNoSuchBinRel bid = failsWithError (NoSuchBinRel bid)

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

instance Arbitrary BinRelId where
  arbitrary = binRelId `liftM` arbitrary
