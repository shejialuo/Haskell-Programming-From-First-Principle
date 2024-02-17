module Chapter3.Exercise where

thirdLetter :: String -> Char
thirdLetter xs = (!!) xs 2

rvrs :: String -> String
rvrs [] = []
rvrs xs = rvrs (drop 1 xs) ++ take 1 xs
