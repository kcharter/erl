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
            quickCheckResult prop_destroyEntitySet,
            quickCheckResult prop_createEntity1,
            quickCheckResult prop_createEntity2,
            quickCheckResult prop_deleteEntity1,
            quickCheckResult prop_deleteEntity2]

prop_createEntitySet :: ErlState Int -> Bool
prop_createEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    maybe (return False) (return . ES.isEmpty) =<< lookupEntitySet esid

prop_destroyEntitySet :: ErlState Int -> Bool
prop_destroyEntitySet s =
  checkErl s $ do
    esid <- createEntitySet
    destroyEntitySet esid
    maybe (return True) (const (return False)) =<< lookupEntitySet esid

prop_createEntity1 :: (ErlState Int, Int) -> Bool
prop_createEntity1 (s, val) =
  checkErl s $ do
    id <- createEntity val
    maybe (return False) (return . checkCreated id) =<< lookupEntity id
  where checkCreated id e = E.id e == id && E.attributes e == val

prop_createEntity2 :: (ErlState Int, Int) -> Bool
prop_createEntity2 (s, val) =
  checkErl s $ do
    id <- createEntity val
    ES.member id `liftM` selectEntities (const True)

prop_deleteEntity1 :: (ErlState Int, E.EntityId) -> Bool
prop_deleteEntity1 (s, id) = do
  checkErl s $ do
    deleteEntity id
    maybe True (const False) `liftM` lookupEntity id

prop_deleteEntity2 :: (ErlState Int, E.EntityId) -> Bool
prop_deleteEntity2 (s, id) = do
  checkErl s $ do
    deleteEntity id
    (not . ES.member id) `liftM` selectEntities (const True)

checkErl :: ErlState d -> Erl d Bool -> Bool
checkErl s erl =
  either (const False) id $ evalErl erl s

instance (Arbitrary d) => Arbitrary (ErlState d) where
  arbitrary = do
    s1 <- foldr addEntitySet emptyState `liftM` arbitrary
    foldr addEntity s1 `liftM` arbitrary
      where addEntitySet () = execErl createEntitySet
            addEntity = execErl . createEntity

withEntity :: (Arbitrary d) => Gen (ErlState d, E.EntityId)
withEntity = do
  s <- arbitrary
  let ids = either (const ES.empty) id $ evalErl (selectEntities (const True)) s
  id <- if ES.isEmpty ids then arbitrary else elements (ES.toList ids)
  return (s, id)
