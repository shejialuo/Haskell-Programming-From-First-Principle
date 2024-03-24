module Chapter15.OptionalMonoidExerciseTest where

import Chapter15.OptionalMonoidExercise (Optional (Only))
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

testSuite :: Test
testSuite =
  TestList
    [ testOptionalMonoid
    ]
