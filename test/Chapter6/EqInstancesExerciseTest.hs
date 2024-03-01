module Chapter6.EqInstancesExerciseTest where

import Chapter6.EqInstancesExercise
  ( EitherOr (Goodbye, Hello),
    Pair (Pair),
    StringOrInt (TisAString, TisAnInt),
    TisAnInteger (TisAn),
    Tuple (Tuple),
    Which (ThatOne, ThisOne),
  )
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testTisAnInteger :: Test
testTisAnInteger =
  TestList
    [ TestCase $ do
        assertEqual
          "TisAn 5 == TisAn 5"
          True
          $ TisAn 5 == TisAn 5
        assertEqual
          "TisAn 6 == TisAn 5"
          False
          $ TisAn 6 == TisAn 5
        assertEqual
          "TisAn 5 /= TisAn 5"
          False
          $ TisAn 5 /= TisAn 5
        assertEqual
          "TisAn 5 /= TisAn 6"
          True
          $ TisAn 5 /= TisAn 6
    ]

testStringOrInt :: Test
testStringOrInt =
  TestList
    [ TestCase $ do
        assertEqual
          "TisAnInt 5 == TisAnInt 5"
          True
          $ TisAnInt 5 == TisAnInt 5
        assertEqual
          "TisAnInt 5 == TisAnInt 6"
          False
          $ TisAnInt 5 == TisAnInt 6
        assertEqual
          "TisAString \"abc\" == TisAString \"abc\""
          True
          $ TisAString "abc" == TisAString "abc"
        assertEqual
          "TisAString \"abc\" == TisAString \"abcd\""
          False
          $ TisAString "abc" == TisAString "abcd"
    ]

testPair :: Test
testPair =
  TestList
    [ TestCase $ do
        assertEqual
          "Pair 5 6 == Pair 6 5"
          False
          $ (Pair 5 6 :: Pair Integer) == (Pair 6 5 :: Pair Integer)
        assertEqual
          "Pair 5 4 == Pair 5 4"
          True
          $ (Pair 5 4 :: Pair Integer) == (Pair 5 4 :: Pair Integer)
    ]

testTuple :: Test
testTuple =
  TestList
    [ TestCase $ do
        assertEqual
          "Tuple 5 6 == Tuple 6 5"
          False
          $ (Tuple 5 6 :: Tuple Integer Integer) == (Tuple 6 5 :: Tuple Integer Integer)
        assertEqual
          "Tuple 5 4 == Tuple 5 4"
          True
          $ (Tuple 5 4 :: Tuple Integer Integer) == (Tuple 5 4 :: Tuple Integer Integer)
    ]

testWhich :: Test
testWhich =
  TestList
    [ TestCase $ do
        assertEqual
          "ThisOne 1 == ThisOne 2"
          False
          $ (ThisOne 1 :: Which Integer) == (ThisOne 2 :: Which Integer)
        assertEqual
          "ThisOne 1 == ThisOne 1"
          True
          $ (ThisOne 1 :: Which Integer) == (ThisOne 1 :: Which Integer)
        assertEqual
          "ThisOne 1 == ThatOne 1"
          False
          $ (ThisOne 1 :: Which Integer) == (ThatOne 2 :: Which Integer)
        assertEqual
          "ThatOne 2 == ThisOne 2"
          False
          $ (ThatOne 2 :: Which Integer) == (ThisOne 2 :: Which Integer)
    ]

testEitherOr :: Test
testEitherOr =
  TestList
    [ TestCase $ do
        assertEqual
          "Hello 1 == Hello 1"
          True
          $ (Hello 1 :: EitherOr Int Int) == (Hello 1 :: EitherOr Int Int)
        assertEqual
          "Goodbye 1 == Goodbye 1"
          True
          $ (Goodbye 1 :: EitherOr Int Int) == (Goodbye 1 :: EitherOr Int Int)
        assertEqual
          "Hello 1 == Goodbye 1"
          False
          $ (Hello 1 :: EitherOr Int Int) == (Goodbye 1 :: EitherOr Int Int)
    ]

testSuite :: Test
testSuite =
  TestList
    [ testTisAnInteger,
      testStringOrInt,
      testPair,
      testTuple,
      testWhich,
      testEitherOr
    ]
