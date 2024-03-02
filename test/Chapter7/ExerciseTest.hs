module Chapter7.ExerciseTest where

import Chapter7.Exercise (foldBool, foldBool', g, tensDigit)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testTensDigit :: Test
testTensDigit =
  TestList
    [ TestCase $ do
        assertEqual
          "tensDigit 10"
          1
          $ tensDigit (10 :: Int)
        assertEqual
          "tensDigit 11"
          1
          $ tensDigit (11 :: Int)
        assertEqual
          "tensDigit 1"
          0
          $ tensDigit (1 :: Int)
        assertEqual
          "tensDigit 123"
          2
          $ tensDigit (123 :: Int)
    ]

testFoldBool :: Test
testFoldBool =
  TestList
    [ TestCase $ do
        assertEqual
          "foldBool 1 2 True"
          1
          $ foldBool (1 :: Int) (2 :: Int) True
        assertEqual
          "foldBool 1 2 False"
          2
          $ foldBool (1 :: Int) (2 :: Int) False
    ]

testFoldBool' :: Test
testFoldBool' =
  TestList
    [ TestCase $ do
        assertEqual
          "foldBool' 1 2 True"
          1
          $ foldBool' (1 :: Int) (2 :: Int) True
        assertEqual
          "foldBool' 1 2 False"
          2
          $ foldBool' (1 :: Int) (2 :: Int) False
    ]

testG :: Test
testG =
  TestList
    [ TestCase $ do
        assertEqual
          "g (*2) (1 , 2) (2, 2)"
          ((2, 2) :: (Int, Int))
          $ g (* 2) (1, 2)
    ]

testSuite :: Test
testSuite =
  TestList
    [ testTensDigit,
      testFoldBool,
      testFoldBool',
      testG
    ]
