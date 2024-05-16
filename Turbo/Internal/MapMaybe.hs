-- | Lots of `mapMaybe*` variants
module Turbo.Internal.MapMaybe where

import Turbo.Internal.Classes
import Turbo.Operators hiding ((?:>))
import Turbo.RootPrelude

(?:<) :: (Cons ys ys y y) => Maybe y -> ys -> ys
(?:<) Nothing ys = ys
(?:<) (Just y) ys = y `cons` ys

(?:>) :: (Snoc ys ys y y) => ys -> Maybe y -> ys
(?:>) ys Nothing = ys
(?:>) ys (Just y) = ys `snoc` y

-- * `mapMaybe` variants

-- ** With index, with Maybe, inside monad

mapMaybeIxLM :: (Enum i, Uncons xs x, AsEmpty ys, Cons ys ys y y, Monad m) => (i -> x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeIxLM #-}
mapMaybeIxLM f = m (toEnum 0)
  where
    m i xs = case uncons xs of
        Nothing -> return Empty
        Just (x, rs) -> do
            y <- f i x
            ys <- m (succ i) rs
            return $ y ?:< ys

mapMaybeIxLRM :: (Enum i, Uncons xs x, AsEmpty ys, Snoc ys ys y y, Monad m) => (i -> x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeIxLRM #-}
mapMaybeIxLRM f = m (toEnum 0) Empty
  where
    m i ys xs = case uncons xs of
        Nothing -> return ys
        Just (x, rs) -> do
            y <- f i x
            m (succ i) (ys ?:> y) rs

mapMaybeIxRM :: (Enum i, Unsnoc xs x, AsEmpty ys, Snoc ys ys y y, Monad m) => (i -> x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeIxRM #-}
mapMaybeIxRM f = fmap snd . m
  where
    m xs = case unsnoc xs of
        Nothing -> return (toEnum 0, Empty)
        Just (rs, x) -> do
            (i, ys) <- m rs
            y <- f i x
            return (succ i, ys ?:> y)

-- ** With index, with maybe, outside monad

mapMaybeIxL :: (Enum i, Uncons xs x, AsEmpty ys, Cons ys ys y y) => (i -> x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeIxL #-}
mapMaybeIxL f = runIdentity . mapMaybeIxLM (Identity .: f)

mapMaybeIxLR :: (Enum i, Uncons xs x, AsEmpty ys, Snoc ys ys y y) => (i -> x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeIxLR #-}
mapMaybeIxLR f = runIdentity . mapMaybeIxLRM (Identity .: f)

mapMaybeIxR :: (Enum i, Unsnoc xs x, AsEmpty ys, Snoc ys ys y y) => (i -> x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeIxR #-}
mapMaybeIxR f = runIdentity . mapMaybeIxRM (Identity .: f)

-- ** With index, inside monad

mapIxLM :: (Enum i, Uncons xs x, AsEmpty ys, Cons ys ys y y, Monad m) => (i -> x -> m y) -> xs -> m ys
{-# INLINE mapIxLM #-}
mapIxLM f = mapMaybeIxLM (fmap Just .: f)

mapIxLRM :: (Enum i, Uncons xs x, AsEmpty ys, Snoc ys ys y y, Monad m) => (i -> x -> m y) -> xs -> m ys
{-# INLINE mapIxLRM #-}
mapIxLRM f = mapMaybeIxLRM (fmap Just .: f)

mapIxRM :: (Enum i, Unsnoc xs x, AsEmpty ys, Snoc ys ys y y, Monad m) => (i -> x -> m y) -> xs -> m ys
{-# INLINE mapIxRM #-}
mapIxRM f = mapMaybeIxRM (fmap Just .: f)

-- ** With index, outside monad

mapIxL :: (Enum i, Uncons xs x, AsEmpty ys, Cons ys ys y y) => (i -> x -> y) -> xs -> ys
{-# INLINE mapIxL #-}
mapIxL f = runIdentity . mapMaybeIxLM (Identity .: Just .: f)

mapIxLR :: (Enum i, Uncons xs x, AsEmpty ys, Snoc ys ys y y) => (i -> x -> y) -> xs -> ys
{-# INLINE mapIxLR #-}
mapIxLR f = runIdentity . mapMaybeIxLRM (Identity .: Just .: f)

mapIxR :: (Enum i, Unsnoc xs x, AsEmpty ys, Snoc ys ys y y) => (i -> x -> y) -> xs -> ys
{-# INLINE mapIxR #-}
mapIxR f = runIdentity . mapMaybeIxRM (Identity .: Just .: f)

-- ** With maybe, inside monad
mapMaybeLM :: (Uncons xs x, AsEmpty ys, Cons ys ys y y, Monad m) => (x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeLM #-}
mapMaybeLM f = mapMaybeIxLM @Int (const f)

mapMaybeLRM :: (Uncons xs x, AsEmpty ys, Snoc ys ys y y, Monad m) => (x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeLRM #-}
mapMaybeLRM f = mapMaybeIxLRM @Int (const f)

mapMaybeRM :: (Unsnoc xs x, AsEmpty ys, Snoc ys ys y y, Monad m) => (x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeRM #-}
mapMaybeRM f = mapMaybeIxRM @Int (const f)

-- | (!) Monad is applied backwards
mapMaybeRLM :: (Unsnoc xs x, AsEmpty ys, Cons ys ys y y, Monad m) => (x -> m (Maybe y)) -> xs -> m ys
{-# INLINE mapMaybeRLM #-}
mapMaybeRLM f = m Empty
  where
    m ys xs = case unsnoc xs of
        Nothing -> return ys
        Just (rs, x) -> do
            y <- f x
            m (y ?:< ys) rs

-- ** With maybe, outside monad
mapMaybeL :: (Uncons xs x, AsEmpty ys, Cons ys ys y y) => (x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeL #-}
mapMaybeL f = runIdentity . mapMaybeIxLM @Int (const (Identity . f))

mapMaybeLR :: (Uncons xs x, AsEmpty ys, Snoc ys ys y y) => (x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeLR #-}
mapMaybeLR f = runIdentity . mapMaybeIxLRM @Int (const (Identity . f))

mapMaybeR :: (Unsnoc xs x, AsEmpty ys, Snoc ys ys y y) => (x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeR #-}
mapMaybeR f = runIdentity . mapMaybeIxRM @Int (const (Identity . f))

mapMaybeRL :: (Unsnoc xs x, AsEmpty ys, Cons ys ys y y) => (x -> Maybe y) -> xs -> ys
{-# INLINE mapMaybeRL #-}
mapMaybeRL f = runIdentity . mapMaybeRLM (Identity . f)
