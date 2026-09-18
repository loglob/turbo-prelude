{-# LANGUAGE QuantifiedConstraints #-}
module Data.Span.MutSpan (
    MutSpan (),
    populate, populate#,
    read, read#,
    write, write#,
 ) where

import Data.Span.Internal
import GHC.Err (undefined)
import GHC.Exts (copySmallMutableArray#, copyMutableArray#, sameMutableArray#, sameSmallMutableArray#, copyArray#, copySmallArray#, freezeArray#, freezeSmallArray#)
import Turbo.RootPrelude
import GHC.Base (error)
import qualified Data.Span.ArraySpan as A
import Turbo.Internal.Classes
import Turbo.Extra (st')

samePtr :: GenMutArray# s a -> GenMutArray# s a -> Bool
samePtr (# x | #) (# y | #) = isTrue# (sameMutableArray# x y)
samePtr (# | x #) (# | y #) = isTrue# (sameSmallMutableArray# x y)
samePtr _ _ = False

instance Span (MutSpan s a) where
    extends :: Int -> Int -> MutSpan s a -> MutSpan s a
    extends (I# l) (I# r) (MutSpan i n arr) 
        | l `lt#` 0# || r `lt#` 0# = error "Sizes must not be negative"
        | l `gt#` i = error "Size out of bounds"
        -- we cannot (properly) check capacity without a state thread
        | otherwise = MutSpan (i -# l) (n +# l +# r) arr

    bounds :: MutSpan s a -> MutSpan s a -> Maybe (MutSpan s a)
    bounds (MutSpan i n xs) (MutSpan j m ys)
        | samePtr xs ys = let !(# o, l #) = _bounds (# i, n #) (# j, m #) in Just (MutSpan o l xs)
        | otherwise              = Nothing

    isSliceOf :: MutSpan s a -> MutSpan s a -> Maybe Int
    isSliceOf (MutSpan i n xs) (MutSpan j m ys)
        | samePtr xs ys = _isSliceOf (# i, n #) (# j, m #)
        | otherwise              = Nothing

    size :: MutSpan s a -> Int
    size (MutSpan _ n _) = I# n

    overlap :: MutSpan s a -> MutSpan s a -> Maybe (MutSpan s a)
    overlap (MutSpan i n xs) (MutSpan j m ys)
        | samePtr xs ys = case _overlap (# i, n #) (# j, m #) of
            (# _ | #)          -> Nothing
            (# | (# o, l #) #) -> Just (MutSpan o l xs)
        | otherwise              = Nothing

    ptrCmp :: MutSpan s a -> MutSpan s a -> Maybe Ordering
    ptrCmp (MutSpan i _ xs) (MutSpan j _ ys)
        | samePtr xs ys = Just (cmp# i j)
        | otherwise     = Nothing

    slice :: Int -> Int -> MutSpan s a -> MutSpan s a
    slice (I# i) (I# n) (MutSpan j m arr) = case _slice (# i, n #) (# j, m #) of
        (# _ | #) -> error "Slice indices out of bounds"
        (# | o #) -> MutSpan o n arr

instance MutableSpan (MutSpan s a) s a where
    write# :: MutSpan s a -> Int# -> a -> State# s -> State# s
    write# (MutSpan i n g) j x s
        | j `geq#` n = error "Index out of bounds"
        | otherwise  = case g of
            (# a | #) -> writeArray# a (i +# j) x s
            (# | a #) -> writeSmallArray# a (i +# j) x s

    read# :: MutSpan s a -> Int# -> State# s -> (# State# s, a #)
    read# (MutSpan i n g) j s 
        | j `geq#` n = error "Index out of bounds"
        | otherwise  = case g of
            (# a | #) -> readArray# a (i +# j) s
            (# | a #) -> readSmallArray# a (i +# j) s

    -- | Copies the contents of a mutable span into another mutable span
    --   $1 - Destination to copy into
    --   $2 - Source to copy from
    memmove# :: MutSpan s a -> MutSpan s a -> State# s -> (# State# s, Int# #)
    memmove# l@(MutSpan i n dst) r@(MutSpan j m src) = \s -> (# run s, z #) where
        !z = min# n m

        run :: State# s -> State# s
        run = case ptrCmp l r of
            Just GT -> reverseSlowCopy -- destination AFTER source
            Just EQ -> \s -> s -- nothing to do
            Just LT -> slowCopy -- destination before source
            Nothing -> case (# dst, src #) of
                -- first check for primitive copy
                (# (# t | #), (# f | #) #) -> copyMutableArray# f i t j n
                (# (# | t #), (# | f #) #) -> copySmallMutableArray# f i t j n
                -- fall back to slow copy
                _ -> slowCopy

        -- equivalent of left-to-right copy loop
        slowCopy = populate# (MutSpan i z dst) (read# r)
        -- copies one-by-one from right to left
        reverseSlowCopy = revCopyLoop (z -# 1#)
        revCopyLoop :: Int# -> State# s -> State# s
        revCopyLoop k s0 
            | k `lt#` 0# = s0
            | otherwise = let
                !(# s1, x #) = read# r k s0
                !s2 = write# l k x s1
                !s3 = revCopyLoop (k -# 1#) s2 
             in
                s3

    copy :: MutSpan s a -> ST s (MutSpan s a)
    copy src@(MutSpan _ n arr) = do
        new <- ST \s -> case arr of
            (# _ | #) -> let !(# s', arr' #) = newArray# n undefined s      in (# s', MutSpan 0# n (# arr' | #) #)
            (# | _ #) -> let !(# s', arr' #) = newSmallArray# n undefined s in (# s', MutSpan 0# n (# | arr' #) #)
        _ <- memmove new src
        return new

    calloc# :: Int# -> a -> State# s -> (# State# s, MutSpan s a #)
    calloc# n a s0 = let
        !(# s1, arr #) = newSmallArray# n a s0
     in
        (# s1, MutSpan 0# n (# | arr #) #)

    baseSpanOffST :: MutSpan s a -> ST s (MutSpan s a, Int)
    baseSpanOffST (MutSpan o _ g) = ST \s0 -> let
        !(# s1, z #) = case g of
            (# a | #) -> (# s0, sizeofMutableArray# a #)
            (# | a #) -> getSizeofSmallMutableArray# a s0
     in
        (# s1, (MutSpan 0# z g, I# o) #)

instance Copyable (MutSpan s a) s a (ArraySpan a) where    
    memcpy :: MutSpan s a -> ArraySpan a -> ST s ()
    memcpy (MutSpan i n dst) r@(ArraySpan j m src) = let 
        !z = min# n m 
     in case (# dst, src #) of
        (# (# to | #), (# fr | #) #) -> st' (copyArray#      fr j to i z)
        (# (# | to #), (# | fr #) #) -> st' (copySmallArray# fr j to i z)
        -- slow copy
        _ -> populate (MutSpan i z dst) \k -> return (r @!! k)

    freezeCopy :: MutSpan s a -> ST s (ArraySpan a)
    freezeCopy (MutSpan i n g) = ST \s0 -> case g of
        (# a | #) -> let
            !(# s1, xs #) = freezeArray# a i n s0
         in
            (# s1, A.fromArray# xs #)
        (# | a #) -> let
            !(# s1, xs #) = freezeSmallArray# a i n s0
         in
            (# s1, A.fromSArray# xs #)
