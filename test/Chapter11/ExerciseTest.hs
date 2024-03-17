module Chapter11.ExerciseTest where

import Chapter11.Exercise (Expr (Add, Lit), capitalizeParagraph, capitalizeWord, capitalizeWords, eval, isSubseqOf, printExpr)
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

testCapitalizeParagraph :: Test
testCapitalizeParagraph =
  TestList
    [ TestCase $ do
        assertEqual
          "capitalizeParagraph \"blah. woot ha.\""
          "Blah. Woot ha."
          $ capitalizeParagraph "blah. woot ha."
        -- Generate more tests
        assertEqual
          "capitalizeParagraph \"blah. woot ha. blah. woot ha.\""
          "Blah. Woot ha. Blah. Woot ha."
          $ capitalizeParagraph "blah. woot ha. blah. woot ha."
    ]

testEval :: Test
testEval =
  TestList
    [ TestCase $ do
        assertEqual
          "eval (Add (Lit 1) (Lit 9001))"
          9002
          $ eval (Add (Lit 1) (Lit 9001))
        assertEqual
          "eval (Add (Lit 1) (Add (Lit 9001) (Lit 1)))"
          9003
          $ eval (Add (Lit 1) (Add (Lit 9001) (Lit 1)))
        assertEqual
          "eval (Add (Lit 1) (Add (Lit 9001) (Add (Lit 1) (Lit 1))))"
          9004
          $ eval (Add (Lit 1) (Add (Lit 9001) (Add (Lit 1) (Lit 1))))
    ]

testPrintEval :: Test
testPrintEval =
  TestList
    [ TestCase $ do
        assertEqual
          "printExpr (Add (Lit 1) (Lit 9001))"
          "1 + 9001"
          $ printExpr (Add (Lit 1) (Lit 9001))
        assertEqual
          "printExpr (Add (Lit 1) (Add (Lit 9001) (Lit 1)))"
          "1 + 9001 + 1"
          $ printExpr (Add (Lit 1) (Add (Lit 9001) (Lit 1)))
        assertEqual
          "printExpr (Add (Lit 1) (Add (Lit 9001) (Add (Lit 1) (Lit 1))))"
          "1 + 9001 + 1 + 1"
          $ printExpr (Add (Lit 1) (Add (Lit 9001) (Add (Lit 1) (Lit 1))))
        assertEqual
          "printExpr (Add (Add (Lit 1) (Lit 9001)) (Add (Lit 1) (Lit 1)))"
          "1 + 9001 + 1 + 1"
          $ printExpr (Add (Add (Lit 1) (Lit 9001)) (Add (Lit 1) (Lit 1)))
        assertEqual
          "printExpr (Add (Add (Lit 1) (Lit 9001)) (Add (Lit 1) (Add (Lit 1) (Lit 1))))"
          "1 + 9001 + 1 + 1 + 1"
          $ printExpr (Add (Add (Lit 1) (Lit 9001)) (Add (Lit 1) (Add (Lit 1) (Lit 1))))
    ]

testSuite :: Test
testSuite =
  TestList
    [ testIsSubseqOf,
      testCapitalizeWords,
      testCapitalizeWord,
      testCapitalizeParagraph,
      testEval,
      testPrintEval
    ]
