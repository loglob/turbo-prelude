module Data.Or where
import Turbo.Prelude hiding (zipWith, zipWithM, partition)

-- | A sum type akin to logical or rather than exclusive or (i.e. Either)
data Or a b = OnlyLeft a | OnlyRight b | Both a b deriving (Show, Eq, Ord)

instance Functor (Or a) where
    fmap = bimap id

instance Bifunctor Or where
    bimap f _ (OnlyLeft a)  = OnlyLeft (f a)
    bimap _ g (OnlyRight b) = OnlyRight (g b)
    bimap f g (Both a b)    = Both (f a) (g b)

instance Bifoldable Or where
    bifold (OnlyLeft x) = x
    bifold (OnlyRight x) = x
    bifold (Both x y) = x <> y
    bifoldMap f _ (OnlyLeft a)  = f a
    bifoldMap _ g (OnlyRight b) = g b
    bifoldMap f g (Both a b)    = f a <> g b
    bifoldr f _ c (OnlyLeft a)  = a `f` c
    bifoldr _ g c (OnlyRight b) = b `g` c
    bifoldr f g c (Both a b)    = a `f` (b `g` c)
    bifoldl f _ c (OnlyLeft a)  = c `f` a
    bifoldl _ g c (OnlyRight b) = c `g` b
    bifoldl f g c (Both a b)    = (c `f` a) `g` b

instance Bitraversable Or where
    bitraverse f _ (OnlyLeft a)  = OnlyLeft $> f a
    bitraverse _ g (OnlyRight b) = OnlyRight $> g b
    bitraverse f g (Both a b)    = Both $> f a <$> g b

-- | Joins two Maybes into one `Or`
or :: Maybe a -> Maybe b -> Maybe (Or a b)
or Nothing  Nothing  = Nothing
or (Just a) Nothing  = Just (OnlyLeft a)
or Nothing  (Just b) = Just (OnlyRight b)
or (Just a) (Just b) = Just (Both a b)

-- | Unpacks an Or into two maybes, at least one of which is `Just`
unOr :: Or a b -> (Maybe a, Maybe b)
unOr (OnlyLeft  a) = (Just a,  Nothing)
unOr (OnlyRight b) = (Nothing, Just b)
unOr (Both a b)    = (Just a,  Just b)

-- | Accesses the left element of an Or
left :: Or a b -> Maybe a
left = fst .unOr

-- | Accesses the right element of an Or
right :: Or a b -> Maybe b
right = snd .unOr

-- | Deconstructs the `Both` case
both :: Or a b -> Maybe (a,b)
both (OnlyLeft _)  = Nothing
both (OnlyRight _) = Nothing
both (Both a b)    = Just (a,b)

-- | Zips two `Or`s with a joining computation
zipWithM :: Applicative f => (a -> a -> f a) -> (b -> b -> f b) -> Or a b -> Or a b -> f (Or a b)
zipWithM f _ (OnlyLeft x)  (OnlyLeft  y) = OnlyLeft $> f x y
zipWithM _ _ (OnlyLeft x)  (OnlyRight y) = pure$ Both x y
zipWithM f _ (OnlyLeft x)  (Both y z)    = Both $> f x y <$ z
zipWithM _ _ (OnlyRight x) (OnlyLeft  y) = pure$ Both y x
zipWithM _ g (OnlyRight x) (OnlyRight y) = OnlyRight $> g x y
zipWithM _ g (OnlyRight x) (Both y z)    = Both y $> g x z
zipWithM f _ (Both w x)    (OnlyLeft  y) = Both $> f w y <$ x
zipWithM _ g (Both w x)    (OnlyRight y) = Both w $> g x y
zipWithM f g (Both w x)    (Both y z)    = Both $> f w y <$> g x z

-- | Zips two `Or`s with a joining function
zipWith :: (a -> a -> a) -> (b -> b -> b) -> Or a b -> Or a b -> Or a b
zipWith f g = runIdentity .: zipWithM (Identity .: f) (Identity .: g)

-- | Zips two `Or`s using the semigroup operator
zip :: (Semigroup a, Semigroup b) => Or a b -> Or a b -> Or a b
zip = zipWith (<>) (<>)

-- | Completes a non-`Both` value with one of the given computations
completeM :: Applicative f => (a -> f b) -> (b -> f a) -> Or a b -> f (a,b)
completeM f _ (OnlyLeft  a) = a &> f a
completeM _ g (OnlyRight b) = g b <& b
completeM _ _ (Both a b)    = pure (a,b)

-- | Completes a non-`Both` value with one of the given functions
complete :: (a -> b) -> (b -> a) -> Or a b -> (a,b)
complete f g = runIdentity . completeM (Identity .f) (Identity .g)

-- | Collapses left and right together with the given computation if `Both`
collapseWithM :: Applicative f => (a -> a -> f a) -> Or a a -> f a
collapseWithM _ (OnlyLeft  a) = pure a
collapseWithM _ (OnlyRight a) = pure a
collapseWithM f (Both a b)    = f a b

-- | Collapses left and right fields together with the given function if `Both`
collapseWith :: (a -> a -> a) -> Or a a -> a
collapseWith f x = runIdentity (collapseWithM (Identity .: f) x)

-- | Collapses left and right fields together with the semigroup operator
collapse :: Semigroup a => Or a a -> a
collapse = collapseWith (<>)

-- | Partitions a list of `Or`s preserving `Both`
partition' :: [Or a b] -> ([a], [(a,b)], [b])
partition'              []  = ([],[],[])
partition' (OnlyLeft  x:rs) = over _1     (x:) (partition' rs)
partition' (OnlyRight y:rs) = over _3     (y:) (partition' rs)
partition' (Both x y   :rs) = over _2 ((x,y):) (partition' rs)

-- | Partitions a list of `Or`s unpacking `Both`
partition :: [Or a b] -> ([a], [b])
partition              []  = ([],[])
partition (OnlyLeft  x:rs) = first  (x:) (partition rs)
partition (OnlyRight y:rs) = second (y:) (partition rs)
partition (Both x y   :rs) = bimap  (x:) (y:) (partition rs)
