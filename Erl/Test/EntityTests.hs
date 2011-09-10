module Erl.Test.EntityTests where

import Erl.Entity

import Control.Monad (liftM)
import Test.QuickCheck
import Test.QuickCheck.Test (isSuccess)

instance Arbitrary EntityId where
  arbitrary = fromInt `liftM` arbitrary

