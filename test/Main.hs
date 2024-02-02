module Main (main) where

import Chapter9.Test
import Control.Monad
import System.Exit
import Test.HUnit

allTests :: Test
allTests = TestList [Chapter9.Test.testSuite]

main :: IO ()
main = do
  counts <- runTestTT allTests
  Control.Monad.when (failures counts > 0) exitFailure
