-- | Generic lens-based collection manipulation
module Turbo.Internal.ConsUtils where

import Turbo.Internal.Classes
import Turbo.Internal.TH
import Turbo.Operators
import Turbo.RootPrelude

-- * dropWhile*

dropWhileIxM :: (Enum i, Uncons xs x, Monad m) => (i -> x -> m Bool) -> xs -> m xs
{-# INLINE dropWhileIxM #-}
dropWhileIxM f = dwi (toEnum 0)
  where
    dwi i xs = case uncons xs of
        Just (y, ys) ->
            f i y >>= \case
                True -> dwi (succ i) ys
                False -> return xs
        _ -> return xs

-- | Generalized `dropWhile` with an index using `Uncons`
dropWhileIx :: (Enum i, Uncons xs x) => (i -> x -> Bool) -> xs -> xs
{-# INLINE dropWhileIx #-}
dropWhileIx f = runIdentity . dropWhileIxM (Identity .: f)

dropWhileM :: (Uncons xs x, Monad m) => (x -> m Bool) -> xs -> m xs
{-# INLINE dropWhileM #-}
dropWhileM = dropWhileIxM @Int . const

-- | Generalized `dropWhile` using `Uncons`
dropWhile :: (Uncons xs x) => (x -> Bool) -> xs -> xs
{-# INLINE dropWhile #-}
dropWhile = dropWhileIx @Int . const

-- ** dropWhileEnd*

dropWhileEndM :: (Unsnoc xs x, Monad m) => (x -> m Bool) -> xs -> m xs
{-# INLINE dropWhileEndM #-}
dropWhileEndM f xs = case unsnoc xs of
    Nothing -> return xs
    Just (ys, y) ->
        f y >>= \case
            True -> dropWhileEndM f ys
            False -> return xs

-- | Generalized `dropWhileEnd` using `Unsnoc`
dropWhileEnd :: (Unsnoc xs x) => (x -> Bool) -> xs -> xs
{-# INLINE dropWhileEnd #-}
dropWhileEnd f = runIdentity . dropWhileEndM (Identity . f)

-- * takeWhile*

takeWhileIxM :: (Enum i, Uncons xs x, Cons ys ys x x, AsEmpty ys, Monad m) => (i -> x -> m Bool) -> xs -> m ys
{-# INLINE takeWhileIxM #-}
takeWhileIxM f = twi (toEnum 0)
  where
    twi i xs = case uncons xs of
        Just (y, rs) ->
            f i y >>= \case
                True -> fmap (cons y) $ twi (succ i) rs
                False -> return Empty
        _ -> return Empty

-- | Generalized `takeWhile` with an index using `Cons`
takeWhileIx :: (Enum i, Uncons xs x, Cons ys ys x x, AsEmpty ys) => (i -> x -> Bool) -> xs -> ys
{-# INLINE takeWhileIx #-}
takeWhileIx f = runIdentity . takeWhileIxM (Identity .: f)

takeWhileM :: (Uncons xs x, Cons ys ys x x, AsEmpty ys, Monad m) => (x -> m Bool) -> xs -> m ys
{-# INLINE takeWhileM #-}
takeWhileM = takeWhileIxM @Int . const

-- | Generalized `takeWhile` using `Cons`
takeWhile :: (Uncons xs x, Cons ys ys x x, AsEmpty ys) => (x -> Bool) -> xs -> ys
{-# INLINE takeWhile #-}
takeWhile = takeWhileIx @Int . const

-- ** takeEndWhile*

takeEndWhileM :: (Unsnoc xs x, Snoc ys ys x x, AsEmpty ys, Monad m) => (x -> m Bool) -> xs -> m ys
{-# INLINE takeEndWhileM #-}
takeEndWhileM f = tew
  where
    tew xs = case unsnoc xs of
        Just (rs, x) ->
            f x >>= \case
                True -> fmap (`snoc` x) $ tew rs
                False -> return Empty
        Nothing -> return Empty

takeEndWhile :: (Unsnoc xs x, Snoc ys ys x x, AsEmpty ys) => (x -> Bool) -> xs -> ys
{-# INLINE takeEndWhile #-}
takeEndWhile f = runIdentity . takeEndWhileM (Identity . f)

-- * span*

spansIxLM :: (Enum i, Uncons xs x, Cons ys ys x x, Monad m) => (i -> x -> m Bool) -> xs -> ys -> m (ys, xs)
spansIxLM f xs0 ys0 = si (toEnum 0) xs0
  where
    si i xs = case uncons xs of
        Just (x, rs) ->
            f i x >>= \case
                True -> fmap (first (cons x)) $ si (succ i) rs
                False -> return (ys0, xs)
        Nothing -> return (ys0, xs)

spansIxRM :: (Enum i, Uncons xs x, Snoc ys ys x x, Monad m) => (i -> x -> m Bool) -> xs -> ys -> m (ys, xs)
spansIxRM f = si (toEnum 0)
  where
    si i xs ys = case uncons xs of
        Just (x, rs) ->
            f i x >>= \case
                True -> si (succ i) rs (ys `snoc` x)
                False -> return (ys, xs)
        Nothing -> return (ys, xs)

spansEndRM :: (Unsnoc xs x, Snoc ys ys x x, Monad m) => (x -> m Bool) -> xs -> ys -> m (xs, ys)
spansEndRM f xs0 ys0 = se xs0
  where
    se xs = case unsnoc xs of
        Just (rs, x) ->
            f x >>= \case
                True -> fmap (second (`snoc` x)) $ se rs
                False -> return (xs, ys0)
        Nothing -> return (xs, ys0)

spansEndLM :: (Unsnoc xs x, Cons ys ys x x, Monad m) => (x -> m Bool) -> xs -> ys -> m (xs, ys)
spansEndLM f = se
  where
    se xs ys = case unsnoc xs of
        Just (rs, x) ->
            f x >>= \case
                True -> se rs (x `cons` ys)
                False -> return (xs, ys)
        Nothing -> return (xs, ys)

mkAllSpans

-- * finding indices

findIndices :: (Enum i, Uncons xs x) => (x -> Bool) -> xs -> [i]
findIndices f = fi (toEnum 0)
  where
    fi i xs = case uncons xs of
        Nothing -> []
        Just (x, rs) -> let ys = fi (succ i) rs in if f x then i : ys else ys

findIndex :: (Enum i, Uncons xs x) => (x -> Bool) -> xs -> Maybe i
findIndex f xs = case findIndices f xs of
    [] -> Nothing
    (i : _) -> Just i

-- * repeat

repeatL :: (Cons xs xs x x) => x -> xs
repeatL x = x `cons` repeatL x

repeatR :: (Snoc xs xs x x) => x -> xs
repeatR x = repeatR x `snoc` x

-- * replicate

replicateL :: (Enum i, AsEmpty xs, Cons xs xs x x) => i -> x -> xs
replicateL n _ | fromEnum n <= 0 = Empty
replicateL n x = x `cons` replicateL (pred n) x

replicateR :: (Enum i, AsEmpty xs, Snoc xs xs x x) => i -> x -> xs
replicateR n _ | fromEnum n <= 0 = Empty
replicateR n x = replicateR (pred n) x `snoc` x

-- * showList__ variants

-- | Shows a list with functions for elements and for intercalating, via left-reduction
showsListL :: (Uncons xs x) => (x -> ShowS) -> ShowS -> xs -> ShowS
showsListL f i = sh
  where
    sh xs = case uncons xs of
        Nothing -> id
        Just (y, ys) -> f y . sh' ys
    sh' xs = case uncons xs of
        Nothing -> id
        Just (y, ys) -> i . f y . sh' ys

-- | Shows a list with a functions for elements and for intercalating, via right-reduction
showsListR :: (Unsnoc xs x) => (x -> ShowS) -> ShowS -> xs -> ShowS
showsListR f i = sh
  where
    sh xs = case unsnoc xs of
        Nothing -> id
        Just (ys, y) -> sh' ys . f y
    sh' xs = case unsnoc xs of
        Nothing -> id
        Just (ys, y) -> sh' ys . f y . i
