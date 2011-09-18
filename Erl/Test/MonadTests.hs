{-# LANGUAGE TupleSections #-}

module Erl.Test.MonadTests where

import Control.Monad (liftM, liftM2)
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
            quickCheckResult $ forAll withEntityId prop_deleteEntity,
            quickCheckResult prop_createEntitySet,
            quickCheckResult $ forAll withEntitySetId prop_deleteEntitySet,
            quickCheckResult prop_addEntity,
            quickCheckResult prop_removeEntity,
            quickCheckResult prop_createBinRel,
            quickCheckResult $ forAll withBinRelId prop_deleteBinRel,
            quickCheckResult prop_addPair,
            quickCheckResult prop_removePair]

prop_createEntity :: ErlState Int -> Bool
prop_createEntity s =
  checkErl s $ do
    eid <- createEntity
    allOf [hasEntity eid,
           elem eid `liftM` entityIds]

prop_deleteEntity :: (ErlState Int, EntityId) -> Bool
prop_deleteEntity (s, eid) =
  checkErl s $ do
    esids <- memberOfEntitySets eid
    bids  <- memberOfBinRels eid
    if (null esids && null bids) then happyCase else inUseCase esids bids
    where happyCase =
            deletion >>
            noneOf [hasEntity eid,
                    elem eid `liftM` entityIds]
          inUseCase esids bids =
            failsWithEntityInUse eid esids bids deletion
          deletion = deleteEntity eid

prop_createEntitySet :: ErlState Int -> Bool
prop_createEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    allOf [haveEntitySet esid,
           inEntitySetIds esid]

prop_deleteEntitySet :: (ErlState Int, EntitySetId) -> Bool
prop_deleteEntitySet (s, esid) =
  checkErl s $ do
    deleteEntitySet esid
    noneOf [haveEntitySet esid,
            inEntitySetIds esid,
            isAssociatedWithSomeEntity esid]

haveEntitySet :: (MonadErl d m) => EntitySetId -> m Bool
haveEntitySet esid =
  maybe False (const True) `liftM` lookupEntitySet esid

inEntitySetIds :: (MonadErl d m) => EntitySetId -> m Bool
inEntitySetIds esid = elem esid `liftM` entitySetIds

isAssociatedWithSomeEntity :: (MonadErl d m) => EntitySetId -> m Bool
isAssociatedWithSomeEntity esid =
  any (elem esid) `liftM` (mapM memberOfEntitySets =<< entityIds)

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
            allOf [(maybe False (val ==) `liftM` lookupEntity eid esid),
                   isAssociatedWithEntity esid eid]
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
            allOf [(maybe True (const False) `liftM` lookupEntity eid esid),
                   not `liftM` isAssociatedWithEntity esid eid]
          noEntityCase =
            failsWithNoSuchEntity eid removal
          removal = removeEntity eid esid

isAssociatedWithEntity :: (MonadErl d m) => EntitySetId -> EntityId -> m Bool
isAssociatedWithEntity esid eid =
  caseHasEntity eid amongEnclosingSets (return False)
    where amongEnclosingSets = elem esid `liftM` memberOfEntitySets eid

prop_createBinRel :: ErlState Int -> Bool
prop_createBinRel s =
  checkErl s $ do
    bid <- createBinRel
    allOf [haveBinRel bid,
           inBinRelIds bid]

prop_deleteBinRel :: (ErlState Int, BinRelId) -> Bool
prop_deleteBinRel (s, bid) =
  checkErl s $ do
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

failsWithEntityInUse :: (MonadErl d m) => EntityId -> [EntitySetId] -> [BinRelId] -> m a -> m Bool
failsWithEntityInUse eid esids bids = failsWithError (EntityInUse eid esids bids)

failsWithError :: (MonadErl d m) => ErlError -> m a -> m Bool
failsWithError expectedError op =
  (op >> return False) `catchError` (return . (expectedError ==))

instance (Arbitrary d) => Arbitrary (ErlState d) where
  arbitrary = do
    s1 <- foldr createAnEntity emptyState `liftM` arbitrary
    s2 <- foldr createAnEntitySet s1 `liftM` atMost 10 arbitrary
    s3 <- foldr createABinRel s2 `liftM` atMost 10 arbitrary
    s4 <- foldr addSetContents s3 `liftM` contentsForSets s3
    s5 <- foldr addBinRelContents s4 `liftM` contentsForBinRels s4
    return s5
      where createAnEntity () = execErl createEntity
            createAnEntitySet () = execErl createEntitySet
            createABinRel () = execErl createBinRel
            contentsForSets s =
              mapM contentsForSet =<< someEntitySetIds s
              where contentsForSet esid =
                      (esid,) `liftM` liftM2 zip (atMost 20 $ someEntityIds s) arbitrary
            addSetContents (esid, contents) s =
              foldr addToSet s contents
              where addToSet (eid, val) s = execErl (addEntity eid val esid) s
            contentsForBinRels s =
              mapM contentsForBinRel =<< someBinRelIds s
              where contentsForBinRel bid = do
                      xs <- atMost 5 $ someEntityIds s
                      ys <- atMost 5 $ someEntityIds s
                      pairs <- atMost 20 $ subLists [(x,y) | x <- xs, y <- ys]
                      vals  <- arbitrary
                      return (bid, zip pairs vals)
            addBinRelContents (bid, contents) s =
              foldr addToBinRel s contents
              where addToBinRel ((eid1,eid2), val) s =
                      execErl (addPair eid1 eid2 val bid) s


withEntityId :: (Arbitrary d) => Gen (ErlState d, EntityId)
withEntityId = do
  s   <- arbitrary
  eid <- anEntityId s
  return (s, eid)

withEntitySetId :: (Arbitrary d) => Gen (ErlState d, EntitySetId)
withEntitySetId = do
  s    <- arbitrary
  esid <- anEntitySetId s
  return (s, esid)

withEntitySetIdAndVal :: (Arbitrary d) => Gen (ErlState d, EntitySetId, d)
withEntitySetIdAndVal = do
  s     <- arbitrary
  esid  <- anEntitySetId s
  val   <- arbitrary
  return (s, esid, val)

withBinRelId :: (Arbitrary d) => Gen (ErlState d, BinRelId)
withBinRelId = do
  s   <- arbitrary
  bid <- aBinRelId s
  return (s, bid)

anEntityId :: ErlState d -> Gen EntityId
anEntityId s = oneOf s entityIds

anEntitySetId :: ErlState d -> Gen EntitySetId
anEntitySetId s = oneOf s entitySetIds

aBinRelId :: ErlState d -> Gen BinRelId
aBinRelId s = oneOf s binRelIds

someEntityIds :: ErlState d -> Gen [EntityId]
someEntityIds s = someOf s entityIds

someEntitySetIds :: ErlState d -> Gen [EntitySetId]
someEntitySetIds s = someOf s entitySetIds

someBinRelIds :: ErlState d -> Gen [BinRelId]
someBinRelIds s = someOf s binRelIds

oneOf :: (Arbitrary a) => ErlState d -> Erl d [a] -> Gen a
oneOf s op = if null xs then arbitrary else elements xs
  where xs = either (error . show) id $ evalErl op s

someOf :: ErlState d -> Erl d [a] -> Gen [a]
someOf s op = subLists vals
  where vals = either (error . show) id $ evalErl op s

subLists :: [a] -> Gen [a]
subLists [] = return []
subLists xs = listOf $ elements xs

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

atMost :: Int -> Gen a -> Gen a
atMost n g = sized $ \m -> if m <= n then g else resize n g
