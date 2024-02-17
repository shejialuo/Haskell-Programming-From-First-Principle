module Chapter3.Test where

import Chapter3.ExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter3.ExerciseTest.testSuite
    ]
