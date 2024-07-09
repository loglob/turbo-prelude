module Turbo.Internal.Search where

import Turbo.Operators
import Turbo.RootPrelude

-- * Base Implementations

binSeekFalseM, binSeekTrueM :: forall a m. (Monad m) => (a -> m Bool) -> (Int -> m a) -> Int -> Int -> m (Maybe (Int, a))

{- | Accepts:
    * A monotonous predicate
    * An monotonous indexing function
    * An inclusive lower bound
    * An exclusive upper bound

    Returns:
        A computation that searches through the indexer function
        and yields the largest False element.

    Note:
        Indexer and predicate must be monotonous, meaning that `forall i <= j. pred (f i) <= pred (f j)` must hold.
-}
binSeekFalseM pred ind = seek
  where
    seek :: Int -> Int -> m (Maybe (Int, a))
    seek lo hi | lo >= hi = return Nothing
    seek lo hi = do
        let mid = (lo + hi) `div` 2
        x <- ind mid
        pred x >>= \case
            False -> seek (mid + 1) hi <?? Just (mid, x)
            True -> seek lo mid

-- | Variant of `binSeekFalse` that yields the smallest True element
binSeekTrueM pred ind = seek
  where
    seek :: Int -> Int -> m (Maybe (Int, a))
    seek lo hi | lo >= hi = return Nothing
    seek lo hi = do
        let !mid = (lo + hi) `div` 2
        x <- ind mid
        pred x >>= \case
            False -> seek (mid + 1) hi
            True -> seek lo mid <?? Just (mid, x)

-- * Specialization inside Monad

binSeekLeqM, binSeekGeqM :: (Monad m, Ord a) => (Int -> m a) -> a -> Int -> Int -> m (Maybe (Int, a))

-- | `binSeekLeq` inside a monad
binSeekLeqM f x = binSeekFalseM (return . (> x)) f

-- | `binSeekGeq` inside a monad
binSeekGeqM f x = binSeekTrueM (return . (>= x)) f

binSeekM, binSeekLastM :: (Monad m, Ord a) => (Int -> m a) -> a -> Int -> Int -> m (Maybe Int)

-- | `binSeek` inside a monad
binSeekM f x lo hi =
    binSeekLeqM f x lo hi <§ \case
        Just (i, y) | x == y -> Just i
        _ -> Nothing

-- | `binSeekLast` inside a monad
binSeekLastM f x lo hi =
    binSeekGeqM f x lo hi <§ \case
        Just (i, y) | x == y -> Just i
        _ -> Nothing

-- * Specialization outside Monad

binSeekTrue, binSeekFalse :: (a -> Bool) -> (Int -> a) -> Int -> Int -> Maybe (Int, a)

-- | Finds the smallest element that satisfies a monotonous predicate in an ordered list
binSeekTrue f g = runIdentity .: binSeekTrueM (Identity . f) (Identity . g)

-- | Finds the largest element that doesn't satisfy a monotonous predicate in an ordered list
binSeekFalse f g = runIdentity .: binSeekFalseM (Identity . f) (Identity . g)

binSeekLeq, binSeekGeq :: (Ord a) => (Int -> a) -> a -> Int -> Int -> Maybe (Int, a)

-- | Finds the largest element <= to a reference element in an ordered list
binSeekLeq f = runIdentity .:. binSeekLeqM (Identity . f)

-- | Finds the smallest element >= to a reference element in an ordered list
binSeekGeq f = runIdentity .:. binSeekGeqM (Identity . f)

binSeek, binSeekLast :: (Ord a) => (Int -> a) -> a -> Int -> Int -> Maybe Int

-- | Finds the lowest index of an element in an ordered list
binSeek f = runIdentity .:. binSeekM (Identity . f)

-- | Finds the highest index of an element in an ordered list
binSeekLast f = runIdentity .:. binSeekLastM (Identity . f)