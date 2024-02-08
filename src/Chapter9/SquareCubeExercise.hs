module Chapter9.SquareCubeExercise where

mySqr :: [Integer]
mySqr = [x * x | x <- [1 .. 5]]

myCube :: [Integer]
myCube = [y * y * y | y <- [1 .. 5]]

combineTuple :: [(Integer, Integer)]
combineTuple = [(x, y) | x <- mySqr, y <- myCube]

combineTupleBothLessThan50 :: [(Integer, Integer)]
combineTupleBothLessThan50 = [(x, y) | x <- mySqr, x < 50, y <- myCube, y < 50]

tupleCount :: Int
tupleCount = length combineTupleBothLessThan50
