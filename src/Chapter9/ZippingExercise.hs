module Chapter9.ZippingExercise where

zip :: [a] -> [b] -> [(a, b)]
zip = Chapter9.ZippingExercise.zipWith (,)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ _ [] = []
zipWith _ [] _ = []
zipWith func (x : xs) (y : ys) =
  func x y
    : Chapter9.ZippingExercise.zipWith func xs ys
