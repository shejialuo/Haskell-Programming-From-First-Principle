module Chapter15.OptionalMonoidExerciseTest where

import Chapter15.OptionalMonoidExercise (First' (First'), Optional (Only))
import Data.Monoid
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testOptionalMonoid :: Test
testOptionalMonoid =
  TestList
    [ TestCase $ do
        assertEqual
          "Only (Sum 1) `mappend` Only (Sum 1)"
          (Only (Sum {getSum = 2}) :: Optional (Sum Integer))
          $ Only (Sum 1) <> Only (Sum 1)
    ]

testFirst'Monoid :: Test
testFirst'Monoid =
  TestList
    [ TestCase $ do
        assertEqual
          "First' (Only (Sum 1)) `mappend` First' (Only (Sum 1))"
          (First' (Only (Sum {getSum = 2})) :: First' (Sum Integer))
          $ First' (Only (Sum 1)) <> First' (Only (Sum 1))
    ]

testSuite :: Test
testSuite =
  TestList
    [ testOptionalMonoid,
      testFirst'Monoid
    ]
