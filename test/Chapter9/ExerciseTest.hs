module Chapter9.ExerciseTest where

import Chapter9.Exercise
  ( capitalizeAllLetters,
    capitalizeFirstLetter,
    filterUpper,
    getFirstLetterUpper,
    myAny,
    myElem,
    myMaximumBy,
    myMinimumBy,
    myOr,
    myReverse,
    squish,
    squishAgain,
    squishMap,
  )
import Data.Ord (comparing)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testFilterUpper :: Test
testFilterUpper =
  TestList
    [ TestCase $ do
        assertEqual
          "filterUpper \"HbEfLrLxO\""
          "HELLO"
          $ filterUpper "HbEfLrLxO"
        assertEqual
          "filterUpper \"abcdefg\""
          ""
          $ filterUpper "abcdefg"
        assertEqual
          "filterUpper \"HELLO\""
          "HELLO"
          $ filterUpper "HELLO"
        assertEqual
          "filterUpper \"HELLOabc\""
          "HELLO"
          $ filterUpper "HELLO"
    ]

testCapitalizeFirstLetter :: Test
testCapitalizeFirstLetter =
  TestList
    [ TestCase $ do
        assertEqual
          "capitalizeFirstLetter \"abc\""
          "Abc"
          $ capitalizeFirstLetter "abc"
        assertEqual
          "capitalizeFirstLetter \"\""
          ""
          $ capitalizeFirstLetter ""
        assertEqual
          "capitalizeFirstLetter \"A\""
          "A"
          $ capitalizeFirstLetter "A"
        assertEqual
          "capitalizeFirstLetter \"aAbBc\""
          "AAbBc"
          $ capitalizeFirstLetter "aAbBc"
    ]

testCapitalizeAllLetters :: Test
testCapitalizeAllLetters =
  TestList
    [ TestCase $ do
        assertEqual
          "capitalizeAllLetters \"\""
          ""
          $ capitalizeAllLetters ""
        assertEqual
          "capitalizeAllLetters \"abc\""
          "ABC"
          $ capitalizeAllLetters "abc"
        assertEqual
          "capitalizeAllLetters \"bBd\""
          "BBD"
          $ capitalizeAllLetters "bBd"
    ]

testGetFirstLetterUpper :: Test
testGetFirstLetterUpper =
  TestList
    [ TestCase $ do
        assertEqual
          "getFirstLetterUpper \"abc\""
          'A'
          $ getFirstLetterUpper "abc"
        assertEqual
          "getFirstLetterUpper \"EBc\""
          'E'
          $ getFirstLetterUpper "EBc"
    ]

testMyOr :: Test
testMyOr =
  TestList
    [ TestCase $ do
        assertEqual
          "myOr [True, False, True]"
          True
          $ myOr [True, False, True]
        assertEqual
          "myOr [False, False, False]"
          False
          $ myOr [False, False, False]
    ]

testMyAny :: Test
testMyAny =
  TestList
    [ TestCase $ do
        assertEqual
          "myAny even [1, 2, 3]"
          True
          $ myAny even ([1, 2, 3] :: [Int])
        assertEqual
          "myAny even [1, 3, 5]"
          False
          $ myAny even ([1, 3, 5] :: [Int])
    ]

testMyElem :: Test
testMyElem =
  TestList
    [ TestCase $ do
        assertEqual
          "myElem 1 [1, 2, 3]"
          True
          $ myElem 1 ([1, 2, 3] :: [Int])
        assertEqual
          "myElem 4 [1, 2, 3]"
          False
          $ myElem 4 ([1, 2, 3] :: [Int])
    ]

testMyReverse :: Test
testMyReverse =
  TestList
    [ TestCase $ do
        assertEqual
          "myReverse [1, 2, 3]"
          [3, 2, 1]
          $ myReverse ([1, 2, 3] :: [Int])
        assertEqual
          "myReverse [1, 2, 3, 4]"
          [4, 3, 2, 1]
          $ myReverse ([1, 2, 3, 4] :: [Int])
    ]

testMySquish :: Test
testMySquish =
  TestList
    [ TestCase $ do
        assertEqual
          "squish [[1, 2, 3], [4, 5, 6]]"
          [1, 2, 3, 4, 5, 6]
          $ squish ([[1, 2, 3], [4, 5, 6]] :: [[Int]])
        assertEqual
          "squish [[1, 2, 3], [4, 5, 6], [7, 8, 9]]"
          [1, 2, 3, 4, 5, 6, 7, 8, 9]
          $ squish ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]])
    ]

testMySquishMap :: Test
testMySquishMap =
  TestList
    [ TestCase $ do
        assertEqual
          "squishMap (\\x -> [x, x]) [1, 2, 3]"
          [1, 1, 2, 2, 3, 3]
          $ squishMap (\x -> [x, x]) ([1, 2, 3] :: [Int])
        assertEqual
          "squishMap (\\x -> [x, x]) [1, 2, 3, 4]"
          [1, 1, 2, 2, 3, 3, 4, 4]
          $ squishMap (\x -> [x, x]) ([1, 2, 3, 4] :: [Int])
    ]

testSquishAgain :: Test
testSquishAgain =
  TestList
    [ TestCase $ do
        assertEqual
          "squishAgain [[1, 2, 3], [4, 5, 6]]"
          [1, 2, 3, 4, 5, 6]
          $ squishAgain ([[1, 2, 3], [4, 5, 6]] :: [[Int]])
        assertEqual
          "squishAgain [[1, 2, 3], [4, 5, 6], [7, 8, 9]]"
          [1, 2, 3, 4, 5, 6, 7, 8, 9]
          $ squishAgain ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]])
    ]

testMyMaximumBy :: Test
testMyMaximumBy = TestCase $ do
  assertEqual
    "myMaximumBy (comparing length) [[1, 2, 3], [4, 5], [6]]"
    [1, 2, 3]
    $ myMaximumBy (comparing length) ([[1, 2, 3], [4, 5], [6]] :: [[Int]])
  assertEqual
    "myMaximumBy (comparing length) [[1, 2, 3], [4, 5, 6], [7, 8, 9]]"
    [1, 2, 3]
    $ myMaximumBy (comparing length) ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]])

testMyMinimumBy :: Test
testMyMinimumBy = TestCase $ do
  assertEqual
    "myMinimumBy (comparing length) [[1, 2, 3], [4, 5], [6]]"
    [6]
    $ myMinimumBy (comparing length) ([[1, 2, 3], [4, 5], [6]] :: [[Int]])
  assertEqual
    "myMinimumBy (comparing length) [[1, 2, 3], [4, 5, 6], [7, 8, 9]]"
    [1, 2, 3]
    $ myMinimumBy (comparing length) ([[1, 2, 3], [4, 5, 6], [7, 8, 9]] :: [[Int]])

testSuite :: Test
testSuite =
  TestList
    [ testFilterUpper,
      testCapitalizeFirstLetter,
      testCapitalizeAllLetters,
      testGetFirstLetterUpper,
      testMyOr,
      testMyAny,
      testMyElem,
      testMyReverse,
      testMySquish,
      testMySquishMap,
      testSquishAgain,
      testMyMaximumBy,
      testMyMinimumBy
    ]
