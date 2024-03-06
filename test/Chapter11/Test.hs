module Chapter11.Test where

import Chapter11.ExerciseTest (testSuite)
import Chapter11.VehiclesExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter11.VehiclesExerciseTest.testSuite,
      Chapter11.ExerciseTest.testSuite
    ]
