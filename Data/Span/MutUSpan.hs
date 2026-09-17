module Data.Span.MutUSpan (
    MutUSpan(),
    fromBytes, fromBytes#,
    memcpy, memcpy#,
    read, read#,
    write, write#,
    populate, populate#,
) where

import Data.Span.Internal
import Turbo.RootPrelude
import GHC.Exts (copyMutableByteArray#, sameMutableByteArray#)
import Data.Primitive (Prim(..), MutableByteArray(..))
import Turbo.Operators ((<&))
import GHC.Err (error)

ptrEq :: MutableByteArray# s -> MutableByteArray# s -> Bool
ptrEq xs ys = isTrue# (sameMutableByteArray# xs ys)

instance Span (MutUSpan s a) where
    extends :: Int -> Int -> MutUSpan s a -> MutUSpan s a
    extends (I# l) (I# r) (MutUSpan i n xs) = case _extends' l r (# i, n #) of
        !(# _ | #) -> error "Invalid extension"
        !(# | (# o, k #) #) -> MutUSpan o k xs
    bounds :: MutUSpan s a -> MutUSpan s a -> Maybe (MutUSpan s a)
    bounds (MutUSpan i n xs) (MutUSpan j m ys) 
        | ptrEq xs ys = let !(# o, l #) = _bounds (# i, n #) (# j, m #) in Just (MutUSpan o l xs)
        | otherwise = Nothing
    isSliceOf :: MutUSpan s a -> MutUSpan s a -> Maybe Int
    isSliceOf (MutUSpan i n xs) (MutUSpan j m ys) 
        | ptrEq xs ys = _isSliceOf (# i, n #) (# j, m #)
        | otherwise = Nothing
    size :: MutUSpan s a -> Int
    size (MutUSpan _ n _) = I# n
    overlap :: MutUSpan s a -> MutUSpan s a -> Maybe (MutUSpan s a)
    overlap (MutUSpan i n xs) (MutUSpan j m ys) 
        | ptrEq xs ys = case _overlap (# i, n #) (# j, m #) of
            (# _ | #)          -> Nothing
            (# | (# o, l #) #) -> Just (MutUSpan o l xs)
        | otherwise = Nothing
    ptrCmp :: MutUSpan s a -> MutUSpan s a -> Maybe Ordering
    ptrCmp (MutUSpan i _ xs) (MutUSpan j _ ys) 
        | ptrEq xs ys = Just (cmp# i j)
        | otherwise = Nothing
    slice :: Int -> Int -> MutUSpan s a -> MutUSpan s a
    slice (I# i) (I# n) (MutUSpan j m xs) = case _slice (# i, n #) (# j, m #) of
        (# _ | #) -> error "invalid slice index"
        (# | o #) -> MutUSpan o n xs

instance StateBasedSpan MutUSpan where
    baseSpanOffST :: MutUSpan x y -> ST x (MutUSpan x y, Int)
    baseSpanOffST (MutUSpan o _ xs) = ST (fromBytes# xs) <& (I# o)

instance MutableSpan MutUSpan where
    memmove# :: MutUSpan s a -> MutUSpan s a -> State# s -> (# State# s, Int# #)
    memmove# (MutUSpan i n dst) (MutUSpan j m src) s0 = let
        !z = min# n m
        !s1 = copyMutableByteArray# src j dst i z s0
     in
        (# s1, z #)

    read# :: MutUSpan s a -> Int# -> State# s -> (# State# s, a #)
    read# (MutUSpan i n xs) j s
        | j `lt#` 0# || j `geq#` n = error "Index out of bounds"
        | otherwise               = readByteArray# xs (i +# j) s

    write# :: MutUSpan s a -> Int# -> a -> State# s -> State# s
    write# (MutUSpan i n xs) j x s
        | j `lt#` 0# || j `geq#` n = error "Index out of bounds"
        | otherwise               = writeByteArray# xs (i +# j) x s

    populate# :: MutUSpan s a -> (Int# -> State# s -> (# State# s, a #)) -> State# s -> State# s
    populate# xs@(MutUSpan _ n _) get = loop 0#
     where
        loop o s | o `geq#` n = s
        loop o s = let
            !(# s1, x #) = get o s
            !s2 = write# xs o x s1
         in
            loop (inc# o) s2
    
    copy :: MutUSpan x y -> ST x (MutUSpan x y)
    copy = _

    malloc :: Int -> y -> ST x (MutUSpan x y)
    malloc (I# n) y = _

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
