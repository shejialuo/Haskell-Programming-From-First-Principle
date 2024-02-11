module Chapter9.FilteringExercise where

filterBetweenOneAndThirty :: [Integer]
filterBetweenOneAndThirty = filter (\x -> rem x 3 == 0) [1 .. 30]

myFilter :: String -> [String]
myFilter = filter (not . (\x -> x `elem` ["the", "a", "an"])) . words
