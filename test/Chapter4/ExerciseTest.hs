module Chapter4.ExerciseTest where

import Chapter4.Exercise (allAwesome, isPalindrome, myAbs)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testLength :: Test
testLength =
  TestList
    [ TestCase $ do
        assertEqual
          "length [1,2,3,4,5]"
          5
          $ length ([1, 2, 3, 4, 5] :: [Integer])
        assertEqual
          "length [(1,2),(2,3),(3,4)]"
          3
          $ length ([(1, 2), (2, 3), (3, 4)] :: [(Integer, Integer)])
        assertEqual
          "length allAwesome"
          2
          $ length allAwesome
        assertEqual
          "length (concat allAwesome)"
          5
          $ length (concat allAwesome)
    ]

testIsPalindrome :: Test
testIsPalindrome =
  TestList
    [ TestCase $ do
        assertEqual
          "isPalindrome \"a\""
          True
          $ isPalindrome "a"
        assertEqual
          "isPalindrome \"ab\""
          False
          $ isPalindrome "ab"
    ]

testMyAbs :: Test
testMyAbs =
  TestList
    [ TestCase $ do
        assertEqual
          "myAbs 1"
          1
          $ myAbs 1
        assertEqual
          "myAbs -1"
          1
          $ myAbs (-1)
    ]

testSuite :: Test
testSuite =
  TestList
    [ testLength,
      testIsPalindrome
    ]
