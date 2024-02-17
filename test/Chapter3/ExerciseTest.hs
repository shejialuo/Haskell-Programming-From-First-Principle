module Chapter3.ExerciseTest where

import Chapter3.Exercise (rvrs, thirdLetter)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testThirdLetter :: Test
testThirdLetter =
  TestList
    [ TestCase $ do
        assertEqual
          "thirdLetter \"thra\""
          'r'
          $ thirdLetter "thra"
        assertEqual
          "thirdLetter \"Curry is awesome\""
          'r'
          $ thirdLetter "Curry is awesome"
    ]

testRvrs :: Test
testRvrs =
  TestList
    [ TestCase $ do
        assertEqual
          "rvrs \"Test hard\""
          "drah tseT"
          $ rvrs "Test hard"
        assertEqual
          "rvrs \"tttaaabbb\""
          "bbbaaattt"
          $ rvrs "tttaaabbb"
    ]

testSuite :: Test
testSuite =
  TestList
    [ testThirdLetter
    ]
