module Chapter15.Exercise where

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  (<>) :: Trivial -> Trivial -> Trivial
  (<>) _ _ = Trivial

instance Monoid Trivial where
  mempty :: Trivial
  mempty = Trivial

newtype Identity a = Identity a deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  (<>) :: Identity a -> Identity a -> Identity a
  (<>) (Identity x) (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
  mempty :: (Monoid a) => Identity a
  mempty = Identity mempty

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) :: Two a b -> Two a b -> Two a b
  (<>) (Two x1 y1) (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty :: (Monoid a, Monoid b) => Two a b
  mempty = Two mempty mempty

data Three a b c = Three a b c deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c) =>
  Semigroup (Three a b c)
  where
  (<>) :: Three a b c -> Three a b c -> Three a b c
  (<>) (Three x1 y1 z1) (Three x2 y2 z2) = Three (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty :: (Monoid a, Monoid b, Monoid c) => Three a b c
  mempty = Three mempty mempty mempty

data Four a b c d = Four a b c d deriving (Eq, Show)

instance
  (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
  Semigroup (Four a b c d)
  where
  (<>) :: Four a b c d -> Four a b c d -> Four a b c d
  (<>) (Four w1 x1 y1 z1) (Four w2 x2 y2 z2) = Four (w1 <> w2) (x1 <> x2) (y1 <> y2) (z1 <> z2)

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty :: (Monoid a, Monoid b, Monoid c, Monoid d) => Four a b c d
  mempty = Four mempty mempty mempty mempty

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (<>) :: BoolConj -> BoolConj -> BoolConj
  (<>) (BoolConj x) (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
  mempty :: BoolConj
  mempty = BoolConj True

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (<>) :: BoolDisj -> BoolDisj -> BoolDisj
  (<>) (BoolDisj x) (BoolDisj y) = BoolDisj (x || y)

instance Monoid BoolDisj where
  mempty :: BoolDisj
  mempty = BoolDisj False
