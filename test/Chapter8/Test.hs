module Chapter8.Test where

import Chapter8.ExerciseTest (testSuite)
import Test.HUnit (Test (TestList))

testSuite :: Test
testSuite = TestList [Chapter8.ExerciseTest.testSuite]
