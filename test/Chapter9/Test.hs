module Chapter9.Test where

import Chapter9.EnumExerciseTest (testSuite)
import Chapter9.FearfulSymmetryExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter9.EnumExerciseTest.testSuite,
      Chapter9.FearfulSymmetryExerciseTest.testSuite
    ]