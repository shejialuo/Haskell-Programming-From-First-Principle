module Chapter7.Test where

import Chapter7.ExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [Chapter7.ExerciseTest.testSuite]
