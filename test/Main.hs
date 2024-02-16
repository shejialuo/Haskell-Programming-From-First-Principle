module Main (main) where

import Chapter8.Test
import Chapter9.Test
import Control.Monad
import System.Exit
import Test.HUnit

allTests :: Test
allTests =
  TestList
    [ Chapter8.Test.testSuite,
      Chapter9.Test.testSuite
    ]

main :: IO ()
main = do
  counts <- runTestTT allTests
  Control.Monad.when (failures counts > 0) exitFailure
