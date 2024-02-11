module Chapter9.FilteringExerciseTest where

import Chapter9.FilteringExercise (myFilter)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testMyFilter :: Test
testMyFilter =
  TestList
    [ TestCase $ do
        assertEqual
          "myFilter \"the brown dog was a goof\""
          ["brown", "dog", "was", "goof"]
          $ myFilter "the brown dog was a goof"
        assertEqual
          "myFilter \"the a a a an a an the\""
          []
          $ myFilter "the a a a an a an the"
    ]

testSuite :: Test
testSuite = TestList [testMyFilter]