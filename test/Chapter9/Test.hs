module Chapter9.Test where

import Chapter9.EnumExerciseTest (testSuite)
import Test.HUnit

testSuite :: Test
testSuite =
  TestList [Chapter9.EnumExerciseTest.testSuite]