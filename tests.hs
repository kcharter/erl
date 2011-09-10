module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

import qualified Erl.Test.BinRelTests as BRT
import qualified Erl.Test.MonadTests as MT

main :: IO ()
main = do
  results <- sequence [BRT.runTests,
                       MT.runTests]
  unless (and results) exitFailure

