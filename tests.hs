module Main where

import Control.Monad (unless, liftM)
import Control.Monad.Error (catchError)
import Data.String
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import qualified Erl.Entity as E
import Erl.Monad

main :: IO ()
main = do
  rs <- sequence [quickCheckResult prop_createEntity1,
                  quickCheckResult prop_createEntity2,
                  quickCheckResult prop_deleteEntity1,
                  quickCheckResult prop_deleteEntity2]
  unless (all isSuccess rs) exitFailure

prop_createEntity1 :: (ErlTState Int, Int) -> Bool
prop_createEntity1 (s, val) =
  checkErl s $ do
    id <- createEntity val
    maybe (return False) (return . checkCreated id) =<< lookupEntity id
  where checkCreated id e = E.id e == id && E.attributes e == val

prop_createEntity2 :: (ErlTState Int, Int) -> Bool
prop_createEntity2 (s, val) =
  checkErl s $ do
    id <- createEntity val
    elem id `liftM` selectEntities (const True)

prop_deleteEntity1 :: (ErlTState Int, E.EntityId) -> Bool
prop_deleteEntity1 (s, id) = do
  checkErl s $ do
    deleteEntity id
    maybe True (const False) `liftM` lookupEntity id

prop_deleteEntity2 :: (ErlTState Int, E.EntityId) -> Bool
prop_deleteEntity2 (s, id) = do
  checkErl s $ do
    deleteEntity id
    (not . elem id) `liftM` selectEntities (const True)

checkErl :: ErlTState d -> ErlMonad d Bool -> Bool
checkErl s erl =
  either (const False) id $ evalErl erl s

instance (Arbitrary d) => Arbitrary (ErlTState d) where
  arbitrary = foldr addEntity emptyState `liftM` arbitrary
    where addEntity = execErl . createEntity

instance Arbitrary E.EntityId where
  arbitrary = E.EntityId `liftM` arbitrary

withEntity :: (Arbitrary d) => Gen (ErlTState d, E.EntityId)
withEntity = do
  s <- arbitrary
  let ids = either (const []) id $ evalErl (selectEntities (const True)) s
  id <- if null ids then arbitrary else elements ids
  return (s, id)
