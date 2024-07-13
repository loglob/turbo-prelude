module Turbo.Extra.List where

import Turbo.Operators
import Turbo.RootPrelude
import qualified Data.List as Ls

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
