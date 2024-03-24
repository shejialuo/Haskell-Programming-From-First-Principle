module Chapter15.Test where

import Chapter15.ExerciseTest (testSuite)
import Chapter15.OptionalMonoidExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter15.OptionalMonoidExerciseTest.testSuite,
      Chapter15.ExerciseTest.testSuite
    ]
