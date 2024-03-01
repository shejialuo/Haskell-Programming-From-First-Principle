module Chapter6.Test where

import Chapter6.EqInstancesExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter6.EqInstancesExerciseTest.testSuite
    ]
