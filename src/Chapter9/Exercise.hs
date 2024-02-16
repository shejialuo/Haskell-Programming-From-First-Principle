module Chapter9.Exercise where

import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (x : xs) = toUpper x : xs

capitalizeAllLetters :: String -> String
capitalizeAllLetters = map toUpper

getFirstLetterUpper :: String -> Char
getFirstLetterUpper = head . capitalizeFirstLetter

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny func = myOr . map func

myElem :: (Eq a) => a -> [a] -> Bool
myElem element = foldr (\x -> (||) (element == x)) False

myReverse :: [a] -> [a]
myReverse xs = reverseHelper xs []
  where
    reverseHelper [] acc = acc
    reverseHelper (y : ys) acc = reverseHelper ys (y : acc)

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap func = foldr ((++) . func) []

squishAgain :: [[a]] -> [a]
squishAgain = undefined

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy = undefined

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy = undefined

myMaximum :: (Ord a) => [a] -> a
myMaximum = undefined

myMinimum :: (Ord a) => [a] -> a
myMinimum = undefined
