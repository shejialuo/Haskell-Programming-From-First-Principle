module Chapter11.ExerciseTest where

import Chapter11.Exercise (capitalizeWord, capitalizeWords, isSubseqOf)
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testIsSubseqOf :: Test
testIsSubseqOf =
  TestList
    [ TestCase $ do
        assertEqual
          "isSubseqOf \"blah\" \"blahwoot\""
          True
          $ isSubseqOf "blah" "blahwoot"
        assertEqual
          "isSubseqOf \"blah\" \"wootblah\""
          True
          $ isSubseqOf "blah" "wootblah"
        assertEqual
          "isSubseqOf \"blah\" \"wboloath\""
          True
          $ isSubseqOf "blah" "wboloath"
        assertEqual
          "isSubseqOf \"blah\" \"wootbla\""
          False
          $ isSubseqOf "blah" "wootbla"
    ]

testCapitalizeWords :: Test
testCapitalizeWords =
  TestList
    [ TestCase $ do
        assertEqual
          "capitalizeWords \"hello world\""
          [("hello", "Hello"), ("world", "World")]
          $ capitalizeWords "hello world"
    ]

testCapitalizeWord :: Test
testCapitalizeWord =
  TestList
    [ TestCase $ do
        assertEqual
          "capitalizeWord \"hello\""
          "Hello"
          $ capitalizeWord "hello"
        assertEqual
          "capitalizeWord \"Hello\""
          "Hello"
          $ capitalizeWord "Hello"
    ]

testSuite :: Test
testSuite =
  TestList
    [ testIsSubseqOf,
      testCapitalizeWords,
      testCapitalizeWord
    ]
