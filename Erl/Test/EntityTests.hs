module Erl.Test.EntityTests where

import Erl.Entity

import Control.Monad (liftM)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

runTests :: IO Bool
runTests = isSuccess `liftM` quickCheckResult prop_fromAndToInt

prop_fromAndToInt :: EntityId -> Bool
prop_fromAndToInt eid = eid == fromInt (toInt eid)

instance Arbitrary EntityId where
  arbitrary = EntityId `liftM` arbitrary

