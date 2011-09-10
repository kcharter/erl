module Erl.Test.MonadTests where

import Control.Monad (unless, liftM)
import Control.Monad.Error (catchError)
import Data.String
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import qualified Erl.Entity as E
import qualified Erl.EntitySet as ES
import Erl.Monad

import Erl.Test.EntityTests ()

runTests :: IO Bool
runTests =
  all isSuccess `liftM`
  sequence [quickCheckResult prop_createEntitySet,
            quickCheckResult prop_deleteEntitySet,
            quickCheckResult $ forAll withEntitySetIdAndVal prop_createEntity,
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

prop_createEntity :: (ErlState Int, EntitySetId, Int) -> Bool
prop_createEntity (s, esid, val) =
  checkErl s $ maybe checkCreateFails checkCreateSucceeds =<< lookupEntitySet esid
    where checkCreateFails =
            checkFails (createEntity esid val)
          checkCreateSucceeds _ = do
            id <- createEntity esid val
            checkAll [maybe False (checkCreated id) `liftM` lookupEntity id,
                      ES.member id `liftM` selectEntities (const True)]
              where checkCreated id e = E.id e == id && E.attributes e == val

prop_deleteEntity :: (ErlState Int, E.EntityId) -> Bool
prop_deleteEntity (s, id) = do
  checkErl s $ do
    deleteEntity id
    checkAll [maybe True (const False) `liftM` lookupEntity id,
              (not . ES.member id) `liftM` selectEntities (const True)]

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
    esids <- sampleEntitySetIds s1
    values <- arbitrary
    return $ foldr addEntity s1 $ zip esids values
      where addEntitySet () = execErl createEntitySet
            addEntity (esid, val) s = execErl (createEntity esid val) s

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

withEntity :: (Arbitrary d) => Gen (ErlState d, E.EntityId)
withEntity = do
  s <- arbitrary
  let ids = either (const ES.empty) id $ evalErl (selectEntities (const True)) s
  id <- if ES.isEmpty ids then arbitrary else elements (ES.toList ids)
  return (s, id)

instance Arbitrary EntitySetId where
  arbitrary = entitySetId `liftM` arbitrary
