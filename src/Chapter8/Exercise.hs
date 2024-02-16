module Chapter8.Exercise where

import Data.List (intersperse)

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

sumFrom1Ton :: (Eq a, Num a) => a -> a
sumFrom1Ton 1 = 1
sumFrom1Ton n = n + sumFrom1Ton (n - 1)

multiplyRecursive :: (Integral a) => a -> a -> a
multiplyRecursive 1 y = y
multiplyRecursive x y = y + multiplyRecursive (x - 1) y

mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 . mc91 $ (n + 11)

digitToWord :: Int -> String
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = error "The input should be 1-9"

digits :: Int -> [Int]
digits n = digitsHelper n []
  where
    digitsHelper 0 acc = acc
    digitsHelper x acc = digitsHelper (div x 10) (mod x 10 : acc)

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
