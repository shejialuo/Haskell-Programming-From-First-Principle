module Chapter9.Test where

import Chapter9.ComprehendThyListsExerciseTest (testSuite)
import Chapter9.EnumExerciseTest (testSuite)
import Chapter9.ExerciseTest (testSuite)
import Chapter9.FearfulSymmetryExerciseTest (testSuite)
import Chapter9.FilteringExerciseTest (testSuite)
import Chapter9.SquareCubeExerciseTest (testSuite)
import Chapter9.ZippingExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter9.EnumExerciseTest.testSuite,
      Chapter9.FearfulSymmetryExerciseTest.testSuite,
      Chapter9.ComprehendThyListsExerciseTest.testSuite,
      Chapter9.SquareCubeExerciseTest.testSuite,
      Chapter9.FilteringExerciseTest.testSuite,
      Chapter9.ZippingExerciseTest.testSuite,
      Chapter9.ExerciseTest.testSuite
    ]