module Chapter9.ZippingExerciseTest where

import Chapter9.ZippingExercise
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testZip :: Test
testZip =
  TestList
    [ TestCase $ do
        assertEqual
          "zip [1] [2]"
          (Prelude.zip [1] [2])
          $ Chapter9.ZippingExercise.zip ([1] :: [Integer]) ([2] :: [Integer])
        assertEqual
          "zip [] []"
          ([] :: [(Integer, Integer)])
          $ Chapter9.ZippingExercise.zip ([] :: [Integer]) ([] :: [Integer])
        assertEqual
          "zip [1, 2] [2, 1]"
          (Prelude.zip [1, 2] [2, 1])
          $ Chapter9.ZippingExercise.zip ([1, 2] :: [Integer]) ([2, 1] :: [Integer])
        assertEqual
          "zip [1, 2] [2]"
          (Prelude.zip [1, 2] [2])
          $ Chapter9.ZippingExercise.zip ([1, 2] :: [Integer]) ([2] :: [Integer])
        assertEqual
          "zip [1] [2, 1]"
          (Prelude.zip [1] [2, 1])
          $ Chapter9.ZippingExercise.zip ([1] :: [Integer]) ([2, 1] :: [Integer])
    ]

testSuite :: Test
testSuite = TestList [testZip]
