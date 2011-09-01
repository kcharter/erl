module Main where

import Control.Monad (unless, liftM)
import Control.Monad.Error (catchError)
import Data.String
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import qualified Erl.EntityType as ET
import qualified Erl.Entity as E
import Erl.Monad
import Erl.Name

main :: IO ()
main = do
  rs <- sequence [quickCheckResult prop_createEntityType1,
                  quickCheckResult prop_createEntityType2,
                  quickCheckResult prop_createEntityType3,
                  quickCheckResult prop_createEntity1,
                  quickCheckResult prop_createEntity2,
                  quickCheckResult prop_deleteEntity1,
                  quickCheckResult prop_deleteEntity2]
  unless (all isSuccess rs) exitFailure

prop_createEntityType1 :: (ErlTState (), Name) -> Bool
prop_createEntityType1 (s, name) = prop_createEntityTypeGeneral canGetIdByName (s, name)
  where canGetIdByName newId = maybe False (newId ==) `liftM` lookupEntityTypeName name

prop_createEntityType2 :: (ErlTState (), Name) -> Bool
prop_createEntityType2 (s, name) = prop_createEntityTypeGeneral idIsInAllIds (s, name)
  where idIsInAllIds newId = elem newId `liftM` entityTypeIds

prop_createEntityType3 :: (ErlTState (), Name) -> Bool
prop_createEntityType3 (s, name) = prop_createEntityTypeGeneral canGetEntityType (s, name)
  where canGetEntityType etId = testEntityType `liftM` entityType etId
          where testEntityType t = ET.id t == etId && ET.name t == name

prop_createEntityTypeGeneral :: (ET.Id -> ErlMonad () Bool) -> (ErlTState (), Name) -> Bool
prop_createEntityTypeGeneral whenNew (s, name)  =
  checkErl s $
    maybe createAndTest checkDuplicateError =<< lookupEntityTypeName name
      where createAndTest = whenNew =<< createEntityType name
            checkDuplicateError _ =
              (createEntityType name >> return False) `catchError` (const $ return True)

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

instance Arbitrary Name where
  arbitrary = do
    base <- elements $ map (:[]) ['A'..'Z']
    suffix <- frequency [(10, return Nothing), (90, Just `liftM` elements [1..100])]
    return $ fromString $ maybe base ((base ++) . show) suffix

instance (Arbitrary d) => Arbitrary (ErlTState d) where
  arbitrary = do
    s1 <- foldr addEntityType emptyState `liftM` arbitrary
    s2 <- foldr addEntity s1 `liftM` arbitrary
    return s2
    where addEntityType = execErl . createEntityType
          addEntity = execErl . createEntity

instance Arbitrary E.EntityId where
  arbitrary = E.EntityId `liftM` arbitrary

withEntity :: (Arbitrary d) => Gen (ErlTState d, E.EntityId)
withEntity = do
  s <- arbitrary
  let ids = either (const []) id $ evalErl (selectEntities (const True)) s
  id <- if null ids then arbitrary else elements ids
  return (s, id)
