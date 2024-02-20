module Chapter4.Exercise where

awesome :: [String]
awesome = ["Papuchon", "curry", ":)"]

also :: [String]
also = ["Quake", "The Simons"]

allAwesome :: [[String]]
allAwesome = [awesome, also]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else -x

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f tuple1 tuple2 = ((snd tuple1, snd tuple2), (fst tuple1, fst tuple2))