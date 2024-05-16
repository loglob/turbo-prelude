-- | Generic lens-based collection manipulation
module Turbo.Internal.ConsUtils where

import Turbo.Internal.Classes
import Turbo.RootPrelude

-- | Generalized `dropWhile` with an index using `Uncons`
dropWhileIx :: (Enum i, Uncons xs x) => (i -> x -> Bool) -> xs -> xs
dropWhileIx f = dwi (toEnum 0)
  where
    dwi i xs = case uncons xs of
        Just (y, ys) | f i y -> dwi (succ i) ys
        _ -> xs

-- | Generalized `dropWhile` using `Uncons`
dropWhile :: (Uncons xs x) => (x -> Bool) -> xs -> xs
dropWhile = dropWhileIx @Int . const

-- | Generalized `dropWhileEnd` using `Unsnoc`
dropWhileEnd :: (Unsnoc xs x) => (x -> Bool) -> xs -> xs
dropWhileEnd f xs = case unsnoc xs of
    Nothing -> xs
    Just (ys, y) -> if f y then dropWhileEnd f ys else xs

-- | Generalized `takeWhile` with an index using `Cons`
takeWhileIx :: (Enum i, Cons xs xs x x, AsEmpty xs) => (i -> x -> Bool) -> xs -> xs
takeWhileIx f = twi (toEnum 0)
  where
    twi i xs = case uncons xs of
        Just (y, rs) | f i y -> y `cons` twi (succ i) rs
        _ -> Empty

-- | Generalized `takeWhile` using `Cons`
takeWhile :: (Cons xs xs x x, AsEmpty xs) => (x -> Bool) -> xs -> xs
takeWhile = takeWhileIx @Int . const

-- | Generalized `span` using `Cons` to construct the left part
spanL :: (Uncons xs x, AsEmpty ys, Cons ys ys x x) => (x -> Bool) -> xs -> (ys, xs)
spanL f xs = case uncons xs of
    Just (x, rs) | f x -> first (cons x) $ spanL f rs
    _ -> (Empty, xs)

-- | Generalized `span` using `Snoc` to construct the left part
spanR :: (Uncons xs x, AsEmpty ys, Snoc ys ys x x) => (x -> Bool) -> xs -> (ys, xs)
spanR f = s Empty
  where
    s ys xs = case uncons xs of
        Just (y, rs) | f y -> s (ys `snoc` y) rs
        _ -> (ys, xs)

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

repeatL :: (Cons xs xs x x) => x -> xs
repeatL x = x `cons` repeatL x

repeatR :: (Snoc xs xs x x) => x -> xs
repeatR x = repeatR x `snoc` x

replicateL :: (Enum i, AsEmpty xs, Cons xs xs x x) => i -> x -> xs
replicateL n _ | fromEnum n <= 0 = Empty
replicateL n x = x `cons` replicateL (pred n) x

replicateR :: (Enum i, AsEmpty xs, Snoc xs xs x x) => i -> x -> xs
replicateR n _ | fromEnum n <= 0 = Empty
replicateR n x = replicateR (pred n) x `snoc` x

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
