module Turbo.Extra.List where

import Data.List qualified as Ls
import Data.Map qualified as M
import Data.Set qualified as S
import Turbo.Operators
import Turbo.RootPrelude

-- | Lens for the head of a NonEmpty
_head1 :: Lens' (NonEmpty a) a
_head1 f (x :| xs) = f x <:| xs

-- | Lens for the possibly-empty tail of a NonEmpty
_tail1 :: Lens' (NonEmpty a) [a]
_tail1 f (x :| xs) = x >:|> f xs

-- | Lens for the last element of a NonEmpty
_last1 :: Lens' (NonEmpty a) a
_last1 f (x :| xs) = g x xs
  where
    g x [] = f x <:| []
    g x (y : ys) = fmap (x <|) (g y ys)

-- | Maps over a list alongside its index
imap :: (Integral i) => (i -> a -> b) -> [a] -> [b]
imap f = m 0
  where
    m _ [] = []
    m i (x : xs) = f i x : m (i + 1) xs

-- | Splits a list by a predicate
split :: (a -> Bool) -> [a] -> NonEmpty [a]
split f = spl
  where
    spl [] = [] :| []
    spl (x : xs) =
        if f x
            then [] <| split f xs
            else let y :| ys = spl xs in (x : y) :| ys

-- | Trims the left and right sides of a list by some removal predicate
trim :: (a -> Bool) -> [a] -> [a]
trim f = Ls.dropWhileEnd f . Ls.dropWhile f

-- | Lifts a function over `NonEmpty` to lists
viaNE :: (NonEmpty a -> b) -> [a] -> Maybe b
viaNE _ [] = Nothing
viaNE f (x : xs) = Just $ f (x :| xs)

{- | Sorts (asc) and nubs a list, invoking a computation to decide which element to pick
    The picking function MUST choose values that are between the two values (as specified by the order relation)
-}
sortNubByM :: forall m a. (Monad m) => (a -> a -> Ordering) -> (a -> a -> m (Maybe a)) -> [a] -> m [a]
sortNubByM cmp pick = partition >=> merge
  where
    merge :: [[a]] -> m [a]
    merge =
        mergeOnce >=> \case
            [] -> return []
            [x] -> return x
            xs -> merge xs

    mergeOnce :: [[a]] -> m [[a]]
    mergeOnce [] = return []
    mergeOnce [x] = return [x]
    mergeOnce !(x : y : zs) = mergePair x y <:> mergeOnce zs

    mergePair :: [a] -> [a] -> m [a]
    mergePair (x : xs) (y : ys) = case cmp x y of
        LT -> x >:> mergePair xs (y : ys)
        GT -> y >:> mergePair (x : xs) ys
        EQ ->
            pick x y >>= \case
                Just z -> mergePair (z : xs) ys
                Nothing -> mergePair xs ys
    mergePair xs [] = return xs
    mergePair [] ys = return ys

    -- \| nubs any adjacent equal values
    nub :: [a] -> m [a]
    nub (x : y : zs)
        | EQ <- cmp x y =
            pick x y >>= \case
                Just z -> nub (z : zs)
                Nothing -> nub zs
    nub (x : ys) = x >:> nub ys
    nub [] = return []

    -- \| partitions input into strictly ascending, nubbed, list segments
    partition :: [a] -> m [[a]]
    partition (x : y : zs) = case cmp y x of
        LT ->
            let
                (ls, rs) = takeDesc (y :| [x]) zs
             in
                nub ls <:> partition rs
        EQ -> do
            z <- pick x y
            partition (z ?: zs)
        GT -> do
            let
                (ls, rs) = takeAsc y zs
             in
                nub (x : ls) <:> partition rs
    partition [x] = return [[x]]
    partition [] = return []

    -- Takes the longest monotonously ascending prefix
    takeAsc :: a -> [a] -> ([a], [a])
    takeAsc l [] = ([l], [])
    takeAsc l (x : xs) = case cmp x l of
        LT -> ([l], x : xs)
        _ -> first (l :) $ takeAsc x xs

    -- Takes the longest strictly descending prefix
    --  Reverses the order to the returned items are in ascending order
    takeDesc :: NonEmpty a -> [a] -> ([a], [a])
    takeDesc ls [] = (toList ls, [])
    takeDesc ls (x : xs) = case cmp x (head ls) of
        GT -> (toList ls, x : xs)
        _ -> takeDesc (x <| ls) xs

sortNubM :: (Monad m, Ord a) => (a -> a -> m (Maybe a)) -> [a] -> m [a]
sortNubM = sortNubByM compare

sortNubBy :: (Ord a) => (a -> a -> Ordering) -> (a -> a -> Maybe a) -> [a] -> [a]
sortNubBy cmp pick = runIdentity . sortNubByM cmp (Identity .: pick)

sortNub :: (Ord a) => (a -> a -> Maybe a) -> [a] -> [a]
sortNub f = runIdentity . sortNubM (Identity .: f)

-- | Variant of `toSetWith` that eliminated duplicates using the given computation
toSetWithM :: (Ord a, Monad m) => (a -> a -> m (Maybe a)) -> [a] -> m (Set a)
toSetWithM pick xs = S.fromDistinctAscList $> sortNubM pick xs

-- | Converts a list to a set. Eliminates duplicate elements using the given picking function.
toSetWith :: (Ord a) => (a -> a -> Maybe a) -> [a] -> Set a
toSetWith pick = runIdentity . toSetWithM (Identity .: pick)

-- | Converts a list to a map. Eliminates duplicate keys using the given picking function.
toMapWithM :: (Ord k, Monad m) => ((k, v) -> (k, v) -> m (Maybe (k, v))) -> [(k, v)] -> m (Map k v)
toMapWithM pick xs = M.fromDistinctAscList $> sortNubByM (\(l, _) (r, _) -> compare l r) pick xs
