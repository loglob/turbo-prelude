module Turbo.Extra.Misc where
import Turbo.RootPrelude
import Turbo.Extra.TH
import Data.Text qualified as T
import qualified Data.Foldable as F

-- * Misc functions

-- | Applies the same function on both sides of a Bifunctor
both :: (Bifunctor f) => (a -> b) -> f a a -> f b b
both f = bimap f f

-- | Access the left element of an Either
left :: Either a b -> Maybe a
left (Left a) = Just a
left (Right _) = Nothing

-- | Access the right element of an Either
right :: Either a b -> Maybe b
right (Left _) = Nothing
right (Right b) = Just b

-- | Applies a function and preserves its argument as the second value
toFst :: (a -> b) -> a -> (b, a)
toFst f x = (f x, x)

-- | Applies a function and preserves its argument as the first value
toSnd :: (a -> b) -> a -> (a, b)
toSnd f x = (x, f x)

-- | Prepends a text's contents before a suffix string
unpacks :: Text -> String -> String
unpacks t x = case T.uncons t of
    Nothing -> x
    Just (c, r) -> c : unpacks r x

-- | Counts the number of elements that fulfill a predicate
count :: (Foldable f, Num i) => (a -> Bool) -> f a -> i
count f = sumBy (\x -> if f x then 1 else 0)

-- | Retrieves the maximum value, if any
maximum :: (Foldable f, Ord a) => f a -> Maybe a
maximum xs = if F.null xs then Nothing else Just (F.maximum xs)

{- | Retrieves the maximum value, if any, by the given ordering function.
  If there are multiple such values, returns the leftmost one.
-}
maximumBy :: (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy f xs = foldr g Nothing xs
  where
    g x Nothing = Just x
    g x (Just y) = Just case f x y of
        GT -> x
        EQ -> x
        LT -> y

-- | Retrieves the minimum value, if any
minimum :: (Foldable f, Ord a) => f a -> Maybe a
minimum xs = if F.null xs then Nothing else Just (F.minimum xs)

{- | Retrieves the minimum value, if any, by the given ordering function.
  If there are multiple such values, returns the leftmost one.
-}
minimumBy :: (Foldable f) => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy f xs = foldr g Nothing xs
  where
    g x Nothing = Just x
    g x (Just y) = Just case f x y of
        GT -> y
        EQ -> x
        LT -> x

-- | Sums a foldable under a mapping
sumBy :: (Foldable f, Num n) => (a -> n) -> f a -> n
sumBy f = foldl' (\x y -> x + f y) 0

-- ** List Operations

-- * Maps over tuples
mkTupleMaps [3 .. 9]

mkTupleColls [3 .. 9]
