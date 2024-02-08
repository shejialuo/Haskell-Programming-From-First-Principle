module Chapter9.FearfulSymmetryExerciseTest (testSuite) where

import Chapter9.FearfulSymmetryExercise (myLines, myWords)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testMyWords :: Test
testMyWords =
  TestList
    [ TestCase $ do
        assertEqual "myWords \"wallfish wants fun\"" ["wallfish", "wants", "fun"] $ myWords "wallfish wants fun"
        assertEqual "myWords \"multiple   spaces\"" ["multiple", "spaces"] $ myWords "multiple   spaces"
    ]

firstSen :: String
firstSen = "Tyger Tyger, burning bright\n"

secondSen :: String
secondSen = "In the forests of the night\n"

thirdSen :: String
thirdSen = "What immortal hand or eye\n"

fourthSen :: String
fourthSen =
  "Could frame thy fearful\
  \ symmetry?"

sentences :: String
sentences = firstSen <> secondSen <> thirdSen <> fourthSen

result :: [String]
result =
  [ "Tyger Tyger, burning bright",
    "In the forests of the night",
    "What immortal hand or eye",
    "Could frame thy fearful symmetry?"
  ]

testMyLines :: Test
testMyLines =
  TestList
    [ TestCase $ do
        assertEqual "myLines sentences" result $ myLines sentences
    ]

testSuite :: Test
testSuite = TestList [testMyWords, testMyLines]
