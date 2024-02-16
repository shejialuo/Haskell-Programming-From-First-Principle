module Chapter9.ExerciseTest where

import Chapter9.Exercise (capitalizeAllLetters, capitalizeFirstLetter, filterUpper, getFirstLetterUpper)
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

testSuite :: Test
testSuite =
  TestList
    [ testFilterUpper,
      testCapitalizeFirstLetter,
      testCapitalizeAllLetters
    ]
