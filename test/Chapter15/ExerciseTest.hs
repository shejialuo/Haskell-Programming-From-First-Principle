module Chapter15.ExerciseTest where

import Chapter15.Exercise (BoolConj (BoolConj), BoolDisj (BoolDisj), Four (Four), Identity (Identity), Three (Three), Trivial (Trivial), Two (Two))
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testSemigroup :: (Eq m, Semigroup m) => m -> m -> m -> Bool
testSemigroup a b c = (a <> (b <> c)) == ((a <> b) <> c)

testLeftIdentity :: (Eq m, Monoid m) => m -> Bool
testLeftIdentity a = (mempty <> a) == a

testRightIdentity :: (Eq m, Monoid m) => m -> Bool
testRightIdentity a = (a <> mempty) == a

testTrivial :: Test
testTrivial =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup Trivial Trivial Trivial"
          True
          $ testSemigroup Trivial Trivial Trivial
        assertEqual
          "testLeftIdentity Trivial"
          True
          $ testLeftIdentity Trivial
        assertEqual
          "testRightIdentity Trivial"
          True
          $ testRightIdentity Trivial
    ]

testIdentity :: Test
testIdentity =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup (Identity Trivial) (Identity Trivial) (Identity Trivial)"
          True
          $ testSemigroup (Identity Trivial) (Identity Trivial) (Identity Trivial)
        assertEqual
          "testLeftIdentity (Identity Trivial)"
          True
          $ testLeftIdentity (Identity Trivial)
        assertEqual
          "testRightIdentity (Identity Trivial)"
          True
          $ testRightIdentity (Identity Trivial)
    ]

testTwo :: Test
testTwo =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup (Two Trivial Trivial) (Two Trivial Trivial) (Two Trivial Trivial)"
          True
          $ testSemigroup (Two Trivial Trivial) (Two Trivial Trivial) (Two Trivial Trivial)
        assertEqual
          "testLeftIdentity (Two Trivial Trivial)"
          True
          $ testLeftIdentity (Two Trivial Trivial)
        assertEqual
          "testRightIdentity (Two Trivial Trivial)"
          True
          $ testRightIdentity (Two Trivial Trivial)
    ]

testThree :: Test
testThree =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup (Three Trivial Trivial Trivial) (Three Trivial Trivial Trivial) (Three Trivial Trivial Trivial)"
          True
          $ testSemigroup (Three Trivial Trivial Trivial) (Three Trivial Trivial Trivial) (Three Trivial Trivial Trivial)
        assertEqual
          "testLeftIdentity (Three Trivial Trivial Trivial)"
          True
          $ testLeftIdentity (Three Trivial Trivial Trivial)
        assertEqual
          "testRightIdentity (Three Trivial Trivial Trivial)"
          True
          $ testRightIdentity (Three Trivial Trivial Trivial)
    ]

testFour :: Test
testFour =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup (Four Trivial Trivial Trivial Trivial) (Four Trivial Trivial Trivial Trivial) (Four Trivial Trivial Trivial Trivial)"
          True
          $ testSemigroup (Four Trivial Trivial Trivial Trivial) (Four Trivial Trivial Trivial Trivial) (Four Trivial Trivial Trivial Trivial)
        assertEqual
          "testLeftIdentity (Four Trivial Trivial Trivial Trivial)"
          True
          $ testLeftIdentity (Four Trivial Trivial Trivial Trivial)
        assertEqual
          "testRightIdentity (Four Trivial Trivial Trivial Trivial)"
          True
          $ testRightIdentity (Four Trivial Trivial Trivial Trivial)
    ]

testBoolConj :: Test
testBoolConj =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup (BoolConj True) (BoolConj True) (BoolConj True)"
          True
          $ testSemigroup (BoolConj True) (BoolConj True) (BoolConj True)
        assertEqual
          "testLeftIdentity (BoolConj True)"
          True
          $ testLeftIdentity (BoolConj True)
        assertEqual
          "testRightIdentity (BoolConj True)"
          True
          $ testRightIdentity (BoolConj True)
    ]

testBoolDisj :: Test
testBoolDisj =
  TestList
    [ TestCase $ do
        assertEqual
          "testSemigroup (BoolDisj True) (BoolDisj True) (BoolDisj True)"
          True
          $ testSemigroup (BoolDisj True) (BoolDisj True) (BoolDisj True)
        assertEqual
          "testLeftIdentity (BoolDisj True)"
          True
          $ testLeftIdentity (BoolDisj True)
        assertEqual
          "testRightIdentity (BoolDisj True)"
          True
          $ testRightIdentity (BoolDisj True)
    ]

testSuite :: Test
testSuite =
  TestList
    [ testTrivial,
      testIdentity,
      testTwo,
      testThree,
      testFour,
      testBoolConj
    ]
