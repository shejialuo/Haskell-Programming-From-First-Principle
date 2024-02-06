module Chapter9.EnumExercise where

{-
  They all follow the same pattern
-}

eftPattern :: (Enum a, Ord a) => a -> a -> [a]
eftPattern start end
  | start > end = []
  | start == end = [start]
  | otherwise = start : eftPattern (succ start) end

eftBool :: Bool -> Bool -> [Bool]
eftBool = eftPattern

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eftPattern

eftInt :: Int -> Int -> [Int]
eftInt = eftPattern

eftChar :: Char -> Char -> [Char]
eftChar = eftPattern
