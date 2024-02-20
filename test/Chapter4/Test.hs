module Chapter4.Test where

import Chapter4.ExerciseTest (testSuite)
import Chapter4.MoodSwingExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite =
  TestList
    [ Chapter4.MoodSwingExerciseTest.testSuite,
      Chapter4.ExerciseTest.testSuite
    ]
