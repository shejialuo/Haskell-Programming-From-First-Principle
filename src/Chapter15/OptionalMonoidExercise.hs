module Chapter15.OptionalMonoidExercise where

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  (<>) :: (Semigroup a) => Optional a -> Optional a -> Optional a
  (<>) Nada x = x
  x <> Nada = x
  (Only x) <> (Only y) = Only (x <> y)

instance (Monoid a) => Monoid (Optional a) where
  mempty :: (Monoid a) => Optional a
  mempty = Nada

newtype First' a = First' {getFirst' :: Optional a}
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (First' a) where
  (<>) :: First' a -> First' a -> First' a
  (<>) (First' (Nada)) x = x
  (<>) x (First' (Nada)) = x
  (<>) (First' x) (First' y) = First' (x <> y)

instance (Monoid a) => Monoid (First' a) where
  mempty :: First' a
  mempty = First' Nada
