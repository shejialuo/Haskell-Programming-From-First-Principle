module Chapter12.Exercise where

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x

replaceThe :: String -> String
replaceThe = unwords . map (maybe "a" id . notThe) . words

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = helper . words
  where
    helper [] = 0
    helper [_] = 0
    helper (x : y : xs) =
      if x == "the" && head y `elem` "aeiou"
        then 1 + helper xs
        else helper (y : xs)

countVowels :: String -> Integer
countVowels = foldl (\acc x -> acc + if x `elem` "aeiou" then 1 else 0) 0

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  if countVowels s > fromIntegral (length s) - countVowels s
    then Nothing
    else Just (Word' s)

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + natToInteger n

integerToNat :: Integer -> Maybe Nat
integerToNat n
  | n < 0 = Nothing
  | otherwise = Just $ helper n
  where
    helper 0 = Zero
    helper x = Succ (helper (x - 1))

isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just y) = f y

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing = x
fromMaybe _ (Just x) = x

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs) = x : catMaybes xs

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr func (Just [])
  where
    func Nothing _ = Nothing
    func _ Nothing = Nothing
    func (Just acc) (Just x) = Just (acc : x)
