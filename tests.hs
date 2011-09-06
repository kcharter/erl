module Main where

import Control.Monad (unless)
import System.Exit (exitFailure)

import qualified Erl.Test.EntityTests as ET
import qualified Erl.Test.BinRelTests as BRT
import qualified Erl.Test.MonadTests as MT

main :: IO ()
main = do
  results <- sequence [ET.runTests,
                       BRT.runTests,
                       MT.runTests]
  unless (and results) exitFailure

