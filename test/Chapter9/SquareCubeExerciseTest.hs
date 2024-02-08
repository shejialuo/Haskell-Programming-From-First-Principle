module Chapter9.SquareCubeExerciseTest (testSuite) where

import Chapter9.SquareCubeExercise (combineTuple, combineTupleBothLessThan50)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testCombineTuple :: Test
testCombineTuple = TestCase $ do
  assertEqual
    "combineTuple"
    [ (1, 1),
      (1, 8),
      (1, 27),
      (1, 64),
      (1, 125),
      (4, 1),
      (4, 8),
      (4, 27),
      (4, 64),
      (4, 125),
      (9, 1),
      (9, 8),
      (9, 27),
      (9, 64),
      (9, 125),
      (16, 1),
      (16, 8),
      (16, 27),
      (16, 64),
      (16, 125),
      (25, 1),
      (25, 8),
      (25, 27),
      (25, 64),
      (25, 125)
    ]
    combineTuple

testCombineTupleBothLessThan50 :: Test
testCombineTupleBothLessThan50 = TestCase $ do
  assertEqual
    "combineTupleBothLessThan50"
    [ (1, 1),
      (1, 8),
      (1, 27),
      (4, 1),
      (4, 8),
      (4, 27),
      (9, 1),
      (9, 8),
      (9, 27),
      (16, 1),
      (16, 8),
      (16, 27),
      (25, 1),
      (25, 8),
      (25, 27)
    ]
    combineTupleBothLessThan50

testSuite :: Test
testSuite = TestList [testCombineTuple, testCombineTupleBothLessThan50]
