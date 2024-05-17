{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Lots of `mapMaybe*` variants
module Turbo.Internal.MapMaybe where

import Turbo.Internal.Classes
import Turbo.Internal.TH
import Turbo.RootPrelude

(?:<) :: (Cons ys ys y y) => Maybe y -> ys -> ys
(?:<) Nothing ys = ys
(?:<) (Just y) ys = y `cons` ys

(?:>) :: (Snoc ys ys y y) => ys -> Maybe y -> ys
(?:>) ys Nothing = ys
(?:>) ys (Just y) = ys `snoc` y

-- * `mapsMaybe` pre-/appending variants

{- | Reduces from left to right, applies an optional mapping using indices, and prepends to a tail
    Computations are evaluated with ascending indices
-}
mapsMaybeIxLM :: (Enum i, Uncons xs x, Cons ys ys y y, Monad m) => (i -> x -> m (Maybe y)) -> xs -> ys -> m ys
{-# INLINE mapsMaybeIxLM #-}
mapsMaybeIxLM f xs0 ys0 = m (toEnum 0) xs0
  where
    m i xs = case uncons xs of
        Nothing -> return ys0
        Just (x, rs) -> do
            y <- f i x
            ys <- m (succ i) rs
            return $ y ?:< ys

{- | Reduces from left to right, applies an optional mapping using indices, and appends to a prefix
    Computations are evaluated with ascending indices
-}
mapsMaybeIxLRM :: (Enum i, Uncons xs x, Snoc ys ys y y, Monad m) => (i -> x -> m (Maybe y)) -> xs -> ys -> m ys
{-# INLINE mapsMaybeIxLRM #-}
mapsMaybeIxLRM f = m (toEnum 0)
  where
    m i xs ys = case uncons xs of
        Nothing -> return ys
        Just (x, rs) -> do
            y <- f i x
            m (succ i) rs (ys ?:> y)

{- | Reduces from right to left, applies an optional mapping using indices, and appends to a prefix
    Computations are evaluated with ascending indices
-}
mapsMaybeIxRM :: (Enum i, Unsnoc xs x, Snoc ys ys y y, Monad m) => (i -> x -> m (Maybe y)) -> xs -> ys -> m ys
{-# INLINE mapsMaybeIxRM #-}
mapsMaybeIxRM f xs0 ys0 = fmap snd $ m xs0
  where
    m xs = case unsnoc xs of
        Nothing -> return (toEnum 0, ys0)
        Just (rs, x) -> do
            (i, ys) <- m rs
            y <- f i x
            return (succ i, ys ?:> y)

mapsMaybeRLM :: (Unsnoc xs x, Cons ys ys y y, Monad m) => (x -> m (Maybe y)) -> xs -> ys -> m ys
{-# INLINE mapsMaybeRLM #-}
mapsMaybeRLM f = m
  where
    m xs ys = case unsnoc xs of
        Nothing -> return ys
        Just (rs, x) -> do
            y <- f x
            m rs (y ?:< ys)

mkAllMaps