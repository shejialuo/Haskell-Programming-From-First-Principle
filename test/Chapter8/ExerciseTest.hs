module Chapter8.ExerciseTest where

import Chapter8.Exercise (appedCatty, cattyConny, flippy, frappe, mc91, multiplyRecursive, sumFrom1Ton, wordNumber)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testCurrying :: Test
testCurrying =
  TestList
    [ TestCase $ do
        assertEqual
          "appedCatty \"woohoo!\""
          "woops mrow woohoo!"
          $ appedCatty "woohoo!"
        assertEqual
          "frappe \"1\""
          "1 mrow haha"
          $ frappe "1"
        assertEqual
          "frappe (appedCatty \"2\")"
          "woops mrow 2 mrow haha"
          $ frappe (appedCatty "2")
        assertEqual
          "cattyConny (frappe \"pink\") (cattyConny \"green\" (appedCatty \"blue\"))"
          "pink mrow haha mrow green mrow woops mrow blue"
          $ cattyConny
            (frappe "pink")
            (cattyConny "green" (appedCatty "blue"))
        assertEqual
          "cattyConny (flippy \"Pugs\" \"are\") \"awesome\""
          "are mrow Pugs mrow awesome"
          $ cattyConny (flippy "Pugs" "are") "awesome"
    ]

testSumFrom1Ton :: Test
testSumFrom1Ton =
  TestList
    [ TestCase $ do
        assertEqual
          "sumFrom1Ton 5"
          (15 :: Integer)
          $ sumFrom1Ton 5
        assertEqual
          "sumFrom1Ton 1"
          (1 :: Integer)
          $ sumFrom1Ton 1
        assertEqual
          "sumFrom1Ton 2"
          (3 :: Integer)
          $ sumFrom1Ton 2
        assertEqual
          "sumFrom1Ton 50"
          (1275 :: Integer)
          $ sumFrom1Ton 50
    ]

testMultiplyRecursive :: Test
testMultiplyRecursive =
  TestList
    [ TestCase $ do
        assertEqual
          "multiplyRecursive 1 1"
          (1 :: Integer)
          $ multiplyRecursive 1 1
        assertEqual
          "multiplyRecursive 3 5"
          (15 :: Integer)
          $ multiplyRecursive 3 5
        assertEqual
          "multiplyRecursive 5 5"
          (25 :: Integer)
          $ multiplyRecursive 5 5
    ]

testMc91 :: Test
testMc91 =
  TestList
    [ TestCase $ do
        assertEqual
          "map mc91 [95..110]"
          ([91, 91, 91, 91, 91, 91, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100] :: [Integer])
          $ map mc91 [95 .. 110]
    ]

testWordNumber :: Test
testWordNumber =
  TestList
    [ TestCase $ do
        assertEqual
          "wordNumber 123"
          "one-two-three"
          $ wordNumber 123
        assertEqual
          "wordNumber 133"
          "one-three-three"
          $ wordNumber 133
        assertEqual
          "wordNumber 123456789"
          "one-two-three-four-five-six-seven-eight-nine"
          $ wordNumber 123456789
    ]

testSuite :: Test
testSuite =
  TestList
    [ testCurrying,
      testSumFrom1Ton,
      testMultiplyRecursive,
      testMc91,
      testWordNumber
    ]
