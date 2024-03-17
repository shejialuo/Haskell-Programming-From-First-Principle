module Chapter11.Exercise where

import Chapter9.Exercise (capitalizeFirstLetter)
import Data.Char (toUpper)

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
capitalizeParagraph = helper True
  where
    helper _ [] = []
    helper _ ('.' : xs) = '.' : helper True xs
    helper state (' ' : xs) = ' ' : helper state xs
    helper True (x : xs) = toUpper x : helper False xs
    helper False (x : xs) = x : helper False xs

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2

printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2
