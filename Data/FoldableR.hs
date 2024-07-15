-- | Alternative for `Data.Foldable` that works for types that aren't `* -> *`
module Data.FoldableR where

import Data.Foldable qualified as F
import Data.Internal.ISpan (size)
import Data.LargeText as LT
import Data.Monoid
import Data.Text qualified as T
import Turbo.Internal.Classes
import Turbo.Operators
import Turbo.RootPrelude

-- * Class definition

{- | Foldable expressed as a relation instead of a predicate
    This permits instances which aren't (* -> *)

    Note that it is a superclass of `Foldable`, so, if possible, you should define a `Foldable` instance instead.
    To specify such an instance, add `import Data.Foldable qualified` to access the hidden class members.
-}
class FoldableR xs x | xs -> x where
    {-# MINIMAL foldMap | foldr #-}
    fold :: (Monoid x) => xs -> x
    fold = foldMap id

    foldMap :: (Monoid y) => (x -> y) -> xs -> y
    foldMap f = foldr (mappend . f) mempty

    foldMap' :: (Monoid y) => (x -> y) -> xs -> y
    foldMap' f = foldl' (\acc a -> acc <> f a) mempty

    foldr :: (x -> y -> y) -> y -> xs -> y
    foldr f z t = appEndo (foldMap (Endo . f) t) z

    foldr' :: (x -> y -> y) -> y -> xs -> y
    foldr' f !y0 !xs = foldr (\x y -> x `seq` y `seq` f x y) y0 xs

    foldl :: (y -> x -> y) -> y -> xs -> y
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z

    foldl' :: (y -> x -> y) -> y -> xs -> y
    {-# INLINE foldl' #-}
    foldl' f !y0 !xs = foldl (\y x -> y `seq` x `seq` f y x) y0 xs

    any, all :: (x -> Bool) -> xs -> Bool
    any p = getAny . foldMap (Any . p)
    all p = getAll . foldMap (All . p)

    toList :: xs -> [x]
    {-# INLINE toList #-}
    toList = foldr (:) []

    null :: xs -> Bool
    null = foldr (\_ _ -> False) True

    length :: (Integral i) => xs -> i
    length = foldl' (const . succ) 0

    elem :: (Eq x) => x -> xs -> Bool
    elem = any . (==)

    sum :: (Num x) => xs -> x
    sum = getSum . foldMap' Sum
    {-# INLINEABLE sum #-}

    product :: (Num x) => xs -> x
    product = getProduct . foldMap' Product
    {-# INLINEABLE product #-}

-- * Auxillary methods

asum :: (FoldableR xs (f a), Alternative f) => xs -> f a
{-# INLINE asum #-}
asum = foldr (<|>) empty

concatMap :: (FoldableR xs x) => (x -> [y]) -> xs -> [y]
concatMap f = foldr ((++) . f) []

concat :: (FoldableR xs [x]) => xs -> [x]
concat = foldr (++) []

find :: (FoldableR xs x) => (x -> Bool) -> xs -> Maybe x
find p = getFirst . foldMap (\x -> First (if p x then Just x else Nothing))

foldlM :: (FoldableR xs x, Monad m) => (y -> x -> m y) -> y -> xs -> m y
foldlM f y0 xs = foldr (\x g y -> f y x >>= g) return xs y0

foldrM :: (FoldableR xs x, Monad m) => (x -> y -> m y) -> y -> xs -> m y
foldrM f y0 xs = foldl (\g x y -> f x y >>= g) return xs y0

forM_ :: (FoldableR xs x, Monad m) => xs -> (x -> m y) -> m ()
{-# INLINE forM_ #-}
forM_ = flip mapM_

for_ :: (FoldableR xs x, Applicative f) => xs -> (x -> f b) -> f ()
{-# INLINE for_ #-}
for_ = flip traverse_

mapM_ :: (FoldableR xs x, Monad m) => (x -> m y) -> xs -> m ()
mapM_ = traverse_

msum :: (FoldableR xs (m a), MonadPlus m) => xs -> m a
{-# INLINE msum #-}
msum = asum

notElem :: (FoldableR xs x, Eq x) => x -> xs -> Bool
notElem = not .: elem

sequenceA_ :: (FoldableR xs (f a), Applicative f) => xs -> f ()
sequenceA_ = traverse_ id

traverse_ :: (FoldableR xs x, Applicative f) => (x -> f b) -> xs -> f ()
traverse_ f = foldr (\x g -> f x <|^> g) (pure ())

-- * Instances

instance {-# OVERLAPPABLE #-} (F.Foldable f) => FoldableR (f x) x where
    fold :: (Monoid x) => f x -> x
    fold = F.fold
    foldMap :: (Monoid y) => (x -> y) -> f x -> y
    foldMap = F.foldMap
    foldMap' :: (Monoid y) => (x -> y) -> f x -> y
    foldMap' = F.foldMap'
    foldr :: (x -> y -> y) -> y -> f x -> y
    foldr = F.foldr
    foldr' :: (x -> y -> y) -> y -> f x -> y
    foldr' = F.foldr'
    foldl :: (y -> x -> y) -> y -> f x -> y
    foldl = F.foldl
    foldl' :: (y -> x -> y) -> y -> f x -> y
    foldl' = F.foldl'
    any :: (x -> Bool) -> f x -> Bool
    any = F.any
    all :: (x -> Bool) -> f x -> Bool
    all = F.all
    toList :: f x -> [x]
    toList = F.toList
    null :: f x -> Bool
    null = F.null
    length :: (Integral i) => f x -> i
    length = toEnum . F.length
    elem :: (Eq x) => x -> f x -> Bool
    elem = F.elem
    sum :: (Num x) => f x -> x
    sum = F.sum
    product :: (Num x) => f x -> x
    product = F.product

instance FoldableR Text Char where
    foldr :: (Char -> y -> y) -> y -> Text -> y
    foldr = T.foldr
    foldr' :: (Char -> y -> y) -> y -> Text -> y
    foldr' = T.foldr'
    foldl :: (y -> Char -> y) -> y -> Text -> y
    foldl = T.foldl
    foldl' :: (y -> Char -> y) -> y -> Text -> y
    foldl' = T.foldl'
    any :: (Char -> Bool) -> Text -> Bool
    any = T.any
    all :: (Char -> Bool) -> Text -> Bool
    all = T.all
    toList :: Text -> [Char]
    toList = T.unpack
    null :: Text -> Bool
    null = T.null
    length :: (Integral i) => Text -> i
    length = toEnum . T.length
    elem :: Char -> Text -> Bool
    elem = T.elem

instance FoldableR LargeText Char where
    foldr :: (Char -> y -> y) -> y -> LargeText -> y
    foldr = T.foldr ..° toText
    foldr' :: (Char -> y -> y) -> y -> LargeText -> y
    foldr' = T.foldr' ..° toText
    foldl :: (y -> Char -> y) -> y -> LargeText -> y
    foldl = T.foldl ..° toText
    foldl' :: (y -> Char -> y) -> y -> LargeText -> y
    foldl' = T.foldl' ..° toText
    any :: (Char -> Bool) -> LargeText -> Bool
    any = T.any .° toText
    all :: (Char -> Bool) -> LargeText -> Bool
    all = T.all .° toText
    toList :: LargeText -> [Char]
    toList = T.unpack . toText
    null :: LargeText -> Bool
    null = (== (0 :: Word)) . length
    length :: (Integral i) => LargeText -> i
    length = toEnum . size

-- ** For Uncons via wrapper

-- | Permits Fold on any Uncons. Not a direct instance because of ambiguity with Unsnoc.
newtype FoldByUncons xs x = FoldByUncons {foldByUncons :: xs}

instance (Uncons xs x) => FoldableR (FoldByUncons xs x) x where
    foldr :: (x -> y -> y) -> y -> FoldByUncons xs x -> y
    foldr f y0 = fr . foldByUncons
      where
        fr xs = case uncons xs of
            Nothing -> y0
            Just (x, xs) -> f x (fr xs)

    foldl :: (y -> x -> y) -> y -> FoldByUncons xs x -> y
    foldl f = fl .° foldByUncons
      where
        fl y xs = case uncons xs of
            Nothing -> y
            Just (x, xs) -> fl (f y x) xs
    null :: FoldByUncons xs x -> Bool
    null = isNothing . uncons . foldByUncons

-- ** For Unsnoc via wrapper

-- | Permits Fold on any Unsnoc. Not a direct instance because of ambiguity with Uncons.
newtype FoldByUnsnoc xs x = FoldByUnsnoc {foldByUnsnoc :: xs}

instance (Unsnoc xs x) => FoldableR (FoldByUnsnoc xs x) x where
    foldr :: (x -> y -> y) -> y -> FoldByUnsnoc xs x -> y
    foldr f = fr .° foldByUnsnoc
      where
        fr y xs = case unsnoc xs of
            Nothing -> y
            Just (xs, x) -> fr (f x y) xs

    foldl :: (y -> x -> y) -> y -> FoldByUnsnoc xs x -> y
    foldl f y0 = fl . foldByUnsnoc
      where
        fl xs = case unsnoc xs of
            Nothing -> y0
            Just (xs, x) -> fl xs `f` x
    null :: FoldByUnsnoc xs x -> Bool
    null = isNothing . unsnoc . foldByUnsnoc
