module Chapter12.Test where

import Chapter12.ExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter12.ExerciseTest.testSuite
    ]
