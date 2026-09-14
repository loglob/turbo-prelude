module Data.Span.MutUSpan (
    MutUSpan(),
    memcpy, memcpy#,
    fromBytes, fromBytes#
) where

import Data.Span.Internal
import Turbo.RootPrelude
import GHC.Exts (copyMutableByteArray#, sameMutableByteArray#)
import Data.Primitive (Prim(..), MutableByteArray(..))
import Turbo.Operators ((<&))
import GHC.Err (error)

memcpy :: MutUSpan s a -> MutUSpan s a -> ST s Int 
memcpy t f = ST \s -> let !(# s', z #) = memcpy# t f s in (# s', I# z #)

memcpy# :: MutUSpan s a -> MutUSpan s a -> State# s -> (# State# s, Int# #)
memcpy# (MutUSpan i n dst) (MutUSpan j m src) s0 = let
    !z = min# n m
    !s1 = copyMutableByteArray# src j dst i z s0
 in
    (# s1, z #)

capacity# :: (Prim a) => Proxy a -> MutableByteArray# s -> State# s -> (# State# s, Int# #)
capacity# p bs s0 = let
    !(# s1, z #) = getSizeofMutableByteArray# bs s0
 in
    (# s1, z `divInt#` sizeOfType# p #)

fromBytes# :: forall s a. (Prim a) => MutableByteArray# s -> State# s -> (# State# s, MutUSpan s a #)
fromBytes# bs s0 = let
    !(# s1, z #) = capacity# (Proxy :: Proxy a) bs s0
 in
    (# s1, MutUSpan 0# z bs #)

fromBytes :: (Prim a) => MutableByteArray s -> ST s (MutUSpan s a)
fromBytes (MutableByteArray bs) = ST (fromBytes# bs)

instance Span (MutUSpan s a) where
    extends :: Int -> Int -> MutUSpan s a -> MutUSpan s a
    extends (I# l) (I# r) (MutUSpan i n xs) = case _extends' l r i n of
        !(# -1#, -1# #) -> error "Invalid extension"
        !(# o, p #) -> MutUSpan o p xs
    bounds :: MutUSpan s a -> MutUSpan s a -> Maybe (MutUSpan s a)
    bounds (MutUSpan i n xs) (MutUSpan j m ys) 
        | isTrue# (sameMutableByteArray# xs ys) = case _bounds i n j m of
            !(# -1#, -1# #) -> Nothing
            (# o, p #) -> Just (MutUSpan o p xs)
        | otherwise = Nothing
    isSliceOf :: MutUSpan s a -> MutUSpan s a -> Maybe Int
    isSliceOf (MutUSpan i n xs) (MutUSpan j m ys) 
        | isTrue# (sameMutableByteArray# xs ys) = _isSliceOf i n j m
        | otherwise = Nothing
    size :: MutUSpan s a -> Int
    size (MutUSpan _ n _) = I# n
    overlap :: MutUSpan s a -> MutUSpan s a -> Maybe (MutUSpan s a)
    overlap (MutUSpan i n xs) (MutUSpan j m ys) 
        | isTrue# (sameMutableByteArray# xs ys) = case _overlap i n j m of
            (# -1#, -1# #) -> Nothing
            (# o, l #) -> Just (MutUSpan o l xs)
        | otherwise = Nothing
    ptrCmp :: MutUSpan s a -> MutUSpan s a -> Maybe Ordering
    ptrCmp (MutUSpan i _ xs) (MutUSpan j _ ys) 
        | isTrue# (sameMutableByteArray# xs ys) = Just (cmp# i j)
        | otherwise = Nothing
    slice :: Int -> Int -> MutUSpan s a -> MutUSpan s a
    slice (I# i) (I# n) (MutUSpan j m xs) = case _slice i n j m of
        -1# -> error "invalid slice index"
        o   -> MutUSpan o n xs

instance StateBasedSpan MutUSpan where
    baseSpanOffST :: MutUSpan x y -> ST x (MutUSpan x y, Int)
    baseSpanOffST (MutUSpan o _ xs) = ST (fromBytes# xs) <& (I# o)
