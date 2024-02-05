module Chapter9.EnumExerciseTest (testSuite) where

import Chapter9.EnumExercise (eftBool, eftChar, eftInt, eftOrd)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testEftBool :: Test
testEftBool =
  TestList
    [ TestCase $ do
        assertEqual "eftBool True False" [True .. False] $ eftBool True False
        assertEqual "eftBool True True" [True .. True] $ eftBool True True
        assertEqual "eftBool False True" [False .. True] $ eftBool False True
        assertEqual "eftBool False False" [False .. False] $ eftBool False False
    ]

testEftOrd :: Test
testEftOrd =
  TestList
    [ TestCase $ do
        assertEqual "eftOrd LT LT" [LT .. LT] $ eftOrd LT LT
        assertEqual "eftOrd GT GT" [GT .. GT] $ eftOrd GT GT
        assertEqual "eftOrd EQ EQ" [EQ .. EQ] $ eftOrd EQ EQ
        assertEqual "eftOrd LT GT" [LT .. GT] $ eftOrd LT GT
        assertEqual "eftOrd LT EQ" [LT .. EQ] $ eftOrd LT EQ
        assertEqual "eftOrd GT LT" [GT .. LT] $ eftOrd GT LT
        assertEqual "eftOrd EQ GT" [EQ .. GT] $ eftOrd EQ GT
        assertEqual "eftOrd GT EQ" [GT .. EQ] $ eftOrd GT EQ
    ]

testEftInt :: Test
testEftInt =
  TestList
    [ TestCase $ do
        assertEqual "eftInt 1 10" [1 .. 10] $ eftInt 1 10
        assertEqual "eftInt 1 -1" [1 .. (-1)] $ eftInt 1 (-1)
        assertEqual "eftInt 1 1" [1 .. 1] $ eftInt 1 1
        assertEqual "eftInt -1 -100" [(-1) .. (-100)] $ eftInt (-1) (-100)
    ]

testEftChar :: Test
testEftChar =
  TestList
    [ TestCase $ do
        assertEqual "eftChar 'a' 'b'" ['a' .. 'b'] $ eftChar 'a' 'b'
        assertEqual "eftChar 'a' 'z'" ['a' .. 'z'] $ eftChar 'a' 'z'
        assertEqual "eftChar 'z' 'z'" ['z' .. 'z'] $ eftChar 'z' 'z'
        assertEqual "eftChar 'b' 'a'" [] $ eftChar 'b' 'a'
        assertEqual "eftChar 'c' 'a'" [] $ eftChar 'c' 'a'
    ]

testSuite :: Test
testSuite = TestList [testEftBool, testEftOrd, testEftInt, testEftChar]
