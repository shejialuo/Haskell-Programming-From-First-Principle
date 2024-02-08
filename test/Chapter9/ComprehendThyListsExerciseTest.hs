module Chapter9.ComprehendThyListsExerciseTest where

import Chapter9.ComprehendThyListsExercise (mySqr)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testMySqr :: Test
testMySqr =
  TestList
    [ TestCase $ do
        assertEqual
          "[x | x <- mySqr, rem x 2 == 0]"
          [4, 16, 36, 64, 100]
          $ [x | x <- mySqr, even x]
        assertEqual
          "[(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]"
          [ (1, 64),
            (1, 81),
            (1, 100),
            (4, 64),
            (4, 81),
            (4, 100),
            (9, 64),
            (9, 81),
            (9, 100),
            (16, 64),
            (16, 81),
            (16, 100),
            (25, 64),
            (25, 81),
            (25, 100),
            (36, 64),
            (36, 81),
            (36, 100),
            (49, 64),
            (49, 81),
            (49, 100)
          ]
          $ [(x, y) | x <- mySqr, x < 50, y <- mySqr, y > 50]
        assertEqual
          "take 5 [(x, y) | x <- mySqr,y <- mySqr, x < 50, y > 50]"
          [(1, 64), (1, 81), (1, 100), (4, 64), (4, 81)]
          $ take 5 [(x, y) | x <- mySqr, x < 50, y <- mySqr, y > 50]
    ]

testSuite :: Test
testSuite = TestList [testMySqr]
