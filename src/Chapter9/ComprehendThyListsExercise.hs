module Chapter9.ComprehendThyListsExercise where

mySqr :: [Integer]
mySqr = [x * x | x <- [1 .. 10]]
