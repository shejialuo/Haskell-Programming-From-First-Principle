module Chapter12.ExerciseTest where

import Chapter12.Exercise
  ( Nat (Succ, Zero),
    catMaybes,
    countTheBeforeVowel,
    countVowels,
    eitherMaybe',
    eitherMaybe'',
    flipMaybe,
    fromMaybe,
    integerToNat,
    isJust,
    isNothing,
    lefts',
    listToMaybe,
    maybeToList,
    mayybee,
    mkWord,
    natToInteger,
    partitionEithers',
    replaceThe,
    right',
  )
import Test.HUnit (Test (TestCase, TestList), assertEqual)

testReplaceThe :: Test
testReplaceThe =
  TestList
    [ TestCase $ do
        assertEqual
          "replaceThe \"the cow loves us\""
          "a cow loves us"
          $ replaceThe "the cow loves us"
        assertEqual
          "replaceThe \"the cow the the the loves us\""
          "a cow a a a loves us"
          $ replaceThe "the cow the the the loves us"
    ]

testCountTheBeforeVowel :: Test
testCountTheBeforeVowel =
  TestList
    [ TestCase $ do
        assertEqual
          "countTheBeforeVowel \"the cow loves us\""
          0
          $ countTheBeforeVowel "the cow loves us"
        assertEqual
          "countTheBeforeVowel \"the cow the the the loves us\""
          0
          $ countTheBeforeVowel "the cow the the the loves us"
        assertEqual
          "countTheBeforeVowel \"the     \""
          0
          $ countTheBeforeVowel "the     "
    ]

testCountVowels :: Test
testCountVowels =
  TestList
    [ TestCase $ do
        assertEqual
          "countVowels \"the cow loves us\""
          5
          $ countVowels "the cow loves us"
        assertEqual
          "countVowels \"the cow the the the loves us\""
          8
          $ countVowels "the cow the the the loves us"
        assertEqual
          "countVowels \"the     \""
          1
          $ countVowels "the     "
    ]

testMkWord :: Test
testMkWord =
  TestList
    [ TestCase $ do
        assertEqual
          "mkWord \"aeu\""
          Nothing
          $ mkWord "aeu"
    ]

testNatToInteger :: Test
testNatToInteger =
  TestList
    [ TestCase $ do
        assertEqual
          "natToInteger Zero"
          0
          $ natToInteger Zero
        assertEqual
          "natToInteger (Succ Zero)"
          1
          $ natToInteger (Succ Zero)
        assertEqual
          "natToInteger (Succ (Succ Zero))"
          2
          $ natToInteger (Succ (Succ Zero))
        assertEqual
          "natToInteger (Succ (Succ (Succ Zero)))"
          3
          $ natToInteger (Succ (Succ (Succ Zero)))
    ]

testIntegerToNat :: Test
testIntegerToNat =
  TestList
    [ TestCase $ do
        assertEqual
          "integerToNat 0"
          (Just Zero)
          $ integerToNat 0
        assertEqual
          "integerToNat 1"
          (Just (Succ Zero))
          $ integerToNat 1
        assertEqual
          "integerToNat 2"
          (Just (Succ (Succ Zero)))
          $ integerToNat 2
    ]

testIsJust :: Test
testIsJust =
  TestList
    [ TestCase $ do
        assertEqual
          "isJust Nothing"
          False
          $ isJust Nothing
        assertEqual
          "isJust (Just 1)"
          True
          $ isJust (Just 1 :: Maybe Int)
    ]

testIsNothing :: Test
testIsNothing =
  TestList
    [ TestCase $ do
        assertEqual
          "isNothing Nothing"
          True
          $ isNothing Nothing
        assertEqual
          "isNothing (Just 1)"
          False
          $ isNothing (Just 1 :: Maybe Int)
    ]

testMayybee :: Test
testMayybee =
  TestList
    [ TestCase $ do
        assertEqual
          "mayybee 0 (+1) Nothing"
          0
          $ mayybee (0 :: Int) (+ 1) Nothing
        assertEqual
          "mayybee 0 (+1) (Just 1)"
          2
          $ mayybee (0 :: Int) (+ 1) (Just 1)
    ]

testFromMaybe :: Test
testFromMaybe =
  TestList
    [ TestCase $ do
        assertEqual
          "fromMaybe 5 Nothing"
          5
          $ fromMaybe (5 :: Int) Nothing
        assertEqual
          "fromMaybe 0 (Just 1)"
          1
          $ fromMaybe (0 :: Int) (Just 1)
    ]

testListToMaybe :: Test
testListToMaybe =
  TestList
    [ TestCase $ do
        assertEqual
          "listToMaybe []"
          Nothing
          $ listToMaybe ([] :: [Int])
        assertEqual
          "listToMaybe [1]"
          (Just 1)
          $ listToMaybe ([1] :: [Int])
    ]

testMaybeToList :: Test
testMaybeToList =
  TestList
    [ TestCase $ do
        assertEqual
          "maybeToList Nothing"
          ([] :: [Int])
          $ maybeToList (Nothing :: Maybe Int)
        assertEqual
          "maybeToList (Just 1)"
          [1]
          $ maybeToList (Just 1 :: Maybe Int)
    ]

testCatMaybes :: Test
testCatMaybes =
  TestList
    [ TestCase $ do
        assertEqual
          "catMaybes [Nothing, Just 1, Nothing, Just 2]"
          [1, 2]
          $ catMaybes [Nothing, Just 1, Nothing, Just 2 :: Maybe Int]
        assertEqual
          "catMaybes [Nothing, Nothing, Nothing]"
          ([] :: [Int])
          $ catMaybes [Nothing, Nothing, Nothing :: Maybe Int]
    ]

testFlipMaybe :: Test
testFlipMaybe =
  TestList
    [ TestCase $ do
        assertEqual
          "flipMaybe [Nothing, Just 1, Nothing, Just 2]"
          Nothing
          $ flipMaybe [Nothing, Just 1, Nothing, Just 2 :: Maybe Int]
        assertEqual
          "flipMaybe [Nothing, Nothing, Nothing]"
          Nothing
          $ flipMaybe [Nothing, Nothing, Nothing :: Maybe Int]
        assertEqual
          "flipMaybe [Just 1, Just 2, Just 3]"
          (Just [1, 2, 3])
          $ flipMaybe [Just 1, Just 2, Just 3 :: Maybe Int]
    ]

testLeft' :: Test
testLeft' =
  TestList
    [ TestCase $ do
        assertEqual
          "lefts' [Left 1, Right 2, Left 3, Right 4]"
          [1, 3]
          $ lefts' [Left 1, Right 2, Left 3, Right 4 :: Either Int Int]
        assertEqual
          "lefts' [Right 2, Right 4]"
          []
          $ lefts' [Right 2, Right 4 :: Either Int Int]
        assertEqual
          "lefts' [Left 1, Left 3]"
          [1, 3]
          $ lefts' [Left 1, Left 3 :: Either Int Int]
    ]

testRight' :: Test
testRight' =
  TestList
    [ TestCase $ do
        assertEqual
          "right' [Left 1, Right 2, Left 3, Right 4]"
          [2, 4]
          $ right' ([Left 1, Right 2, Left 3, Right 4] :: [Either Int Int])
        assertEqual
          "right' [Right 2, Right 4]"
          [2, 4]
          $ right' ([Right 2, Right 4] :: [Either Int Int])
        assertEqual
          "right' [Left 1, Left 3]"
          []
          $ right' ([Left 1, Left 3] :: [Either Int Int])
    ]

testPartitionEithers' :: Test
testPartitionEithers' =
  TestList
    [ TestCase $ do
        assertEqual
          "partitionEithers' [Left 1, Right 2, Left 3, Right 4]"
          ([1, 3], [2, 4])
          $ partitionEithers' [Left 1, Right 2, Left 3, Right 4 :: Either Int Int]
        assertEqual
          "partitionEithers' [Right 2, Right 4]"
          ([], [2, 4])
          $ partitionEithers' [Right 2, Right 4 :: Either Int Int]
        assertEqual
          "partitionEithers' [Left 1, Left 3]"
          ([1, 3], [])
          $ partitionEithers' [Left 1, Left 3 :: Either Int Int]
    ]

testEitherMaybe' :: Test
testEitherMaybe' =
  TestList
    [ TestCase $ do
        assertEqual
          "eitherMaybe' (+1) (Left 1)"
          Nothing
          $ eitherMaybe' (+ 1) (Left 1 :: Either Int Int)
        assertEqual
          "eitherMaybe' (+1) (Right 1)"
          (Just 2)
          $ eitherMaybe' (+ 1) (Right 1 :: Either Int Int)
    ]

testEitherMaybe'' :: Test
testEitherMaybe'' =
  TestList
    [ TestCase $ do
        assertEqual
          "eitherMaybe' (+1) (Left 1)"
          Nothing
          $ eitherMaybe'' (+ 1) (Left 1 :: Either Int Int)
        assertEqual
          "eitherMaybe' (+1) (Right 1)"
          (Just 2)
          $ eitherMaybe' (+ 1) (Right 1 :: Either Int Int)
    ]

testSuite :: Test
testSuite =
  TestList
    [ testReplaceThe,
      testCountTheBeforeVowel,
      testCountVowels,
      testMkWord,
      testNatToInteger,
      testIntegerToNat,
      testIsJust,
      testIsNothing,
      testMayybee,
      testFromMaybe,
      testListToMaybe,
      testCatMaybes,
      testFlipMaybe,
      testLeft',
      testRight',
      testPartitionEithers',
      testEitherMaybe',
      testEitherMaybe''
    ]
