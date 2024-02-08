module Chapter9.FearfulSymmetryExercise (myWords, myLines) where

splitString :: (Char -> Bool) -> String -> [String]
splitString _ [] = []
splitString func strs = substr : splitString func remain
  where
    substr = takeWhile func strs
    remain = dropWhile (not . func) $ dropWhile func strs

myWords :: String -> [String]
myWords = splitString (/= ' ')

myLines :: String -> [String]
myLines = splitString (/= '\n')
