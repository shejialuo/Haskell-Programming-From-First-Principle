module Chapter4.MoodSwingExerciseTest where

import Chapter4.MoodSwingExercise (Mood (Blah, Woot), changeMood)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

blah :: Mood
blah = Blah

woot :: Mood
woot = Woot

testChangeMood :: Test
testChangeMood =
  TestList
    [ TestCase $ do
        assertEqual
          "changeMood woot"
          blah
          $ changeMood woot
        assertEqual
          "changeMood blah"
          woot
          $ changeMood blah
    ]

testSuite :: Test
testSuite = TestList [testChangeMood]
