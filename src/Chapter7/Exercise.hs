module Chapter7.Exercise where

tensDigit :: (Integral a) => a -> a
tensDigit x = snd $ divMod (fst $ divMod x 10) 10

foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
  True -> x
  False -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y b
  | b = x
  | otherwise = y

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a, c)
