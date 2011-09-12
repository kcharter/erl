module Erl.Test.EntityTests where

import Erl.Entity

import Control.Monad (liftM)
import Test.QuickCheck

instance Arbitrary EntityId where
  arbitrary = fromInt `liftM` arbitrary

