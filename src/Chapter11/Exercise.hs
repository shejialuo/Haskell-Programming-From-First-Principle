module Chapter11.Exercise where

import Chapter9.Exercise (capitalizeFirstLetter)

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf subseq@(x : xs) (y : ys)
  | x == y = isSubseqOf xs ys
  | otherwise = isSubseqOf subseq ys

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map (\x -> (x, capitalizeFirstLetter x)) . words

capitalizeWord :: String -> String
capitalizeWord = capitalizeFirstLetter

capitalizeParagraph :: String -> String
capitalizeParagraph = undefined
