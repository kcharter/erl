module Main where

import Control.Monad (unless, liftM)
import Control.Monad.Error (catchError)
import Data.String
import System.Exit (exitFailure)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

import qualified Erl.EntityType as ET
import Erl.Monad
import Erl.Name

main :: IO ()
main = do
  rs <- sequence [quickCheckResult prop_createEntityType1,
                  quickCheckResult prop_createEntityType2,
                  quickCheckResult prop_createEntityType3]
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

checkErl :: ErlTState () -> ErlMonad () Bool -> Bool
checkErl s erl =
  either (const False) id $ evalErl erl s

instance Arbitrary Name where
  arbitrary = do
    base <- elements $ map (:[]) ['A'..'Z']
    suffix <- frequency [(10, return Nothing), (90, Just `liftM` elements [1..100])]
    return $ fromString $ maybe base ((base ++) . show) suffix

instance Arbitrary (ErlTState d) where
  arbitrary = foldr addEntityType emptyState `liftM` arbitrary
    where addEntityType name =
            execErl (createEntityType name)