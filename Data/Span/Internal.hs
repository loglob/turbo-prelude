{-# LANGUAGE QuantifiedConstraints #-}
module Data.Span.Internal where
import Data.Primitive
import GHC.Base
import Turbo.Internal.Classes
import Turbo.Operators ((?!))
import Turbo.RootPrelude

{- | Generic wrapper for either primitive array type.
    Differences should be negligible because they are immutable.
    (I think they are only separate types because they could be thawed again)
-}
type GenArray# (a :: TYPE (BoxedRep l)) = (# Array# a | SmallArray# a #)

-- | Generic wrapper around mutable arrays
type GenMutArray# s (a :: TYPE (BoxedRep l)) = (# MutableArray# s a | SmallMutableArray# s a #)

{- | A segment of an immutable array
 Permits pointer-equality and comparison, rather than structural equality
-}
data ArraySpan (a :: TYPE (BoxedRep l)) 
    -- | Arguments are offset, size, storage
    = ArraySpan Int# Int# (GenArray# a)

-- | A view of a mutable array
data MutSpan s (x :: TYPE (BoxedRep l))
    -- | Arguments are offset, size, storage
    = MutSpan Int# Int# (GenMutArray# s x)

{- | Segment of a byte array.
 Offers more compact and efficient representation, but doesn't support laziness.
-}
data USpan a where
    -- Use a GADT to bind the `Prim` constraint into the constructor, otherwise classes on USpan aren't doable

    -- | Offsets/length in numbers of elements, NOT bytes
    USpan :: (Prim a) => Int# -> Int# -> ByteArray# -> USpan a

data MutUSpan s a where
    MutUSpan :: (Prim a) => Int# -> Int# -> MutableByteArray# s -> MutUSpan s a

-- ** Span class

{- | A collection type that permits comparing the underlying pointers,
 and creating 0-copy slices
-}
class Span s where
    -- | Computes the smallest span that contains both input spans
    --   Returns `Nothing` if they are part of different base spans
    bounds :: s -> s -> Maybe s

    -- | Extends a span to the left and right by the given number.
    --   Partial if indices are out-of-bounds.
    extends :: Int -> Int -> s -> s

    -- | Undoes `slice`, returning its first arguments
    isSliceOf :: s -> s -> Maybe Int

    -- | Computes the largest span that is a slice of both given spans.
    --   Returns `Nothing` when they don't overlap.
    --   Returns an empty span if the spans are exactly next to another.
    overlap :: s -> s -> Maybe s

    -- | Compares the underlying pointers of two spans
    --   Returns `Nothing` if the spans point into different arrays,
    --   compares the starting address of the spans otherwise.
    ptrCmp :: s -> s -> Maybe Ordering

    -- | The length of a span
    size :: s -> Int

    -- | Creates a sub-span from an offset and a length
    --   Partial if indices are out of bounds
    slice :: Int -> Int -> s -> s
    slice n m = takes m . trims n 0

    -- | Like `slice`, but indexes from the end of the span rather than the start.
    --    Partial if indices are out of bounds.
    --
    --    (!) The last element is at index 1
    sliceEnd :: Int -> Int -> s -> s
    sliceEnd n m s = slice (size s - n) m s

    -- | Returns only the $1 leftmost elements of $2
    takes :: Int -> s -> s
    takes = slice 0

    -- | Trims the $1 leftmost and $2 rightmost elements of $3
    trims :: Int -> Int -> s -> s
    trims l r s =
        let z = size s
         in if l < 0 || r < 0 || l + r > z
                then error "trims indices out of range"
                else slice l (size s - l - r) s

    {-# MINIMAL (extends, bounds, isSliceOf, size, overlap, ptrCmp, (slice | (trims, takes))) #-}

-- *** BasedSpan

-- | A span that can be traced back to the baseSpan that contains it
class Span s => BasedSpan s where
    -- | A span of the entire array the input span slices
    baseSpan :: s -> s
    baseSpan = fst . baseSpanOff

    -- | Like `baseSpan` but also returns the starting offset of the input span
    baseSpanOff :: s -> (s, Int)
    baseSpanOff x =
        let
            b = baseSpan x
            o = x `isSliceOf` b
         in
            (b, o ?! error "Span violated slice law")

-- | Variant of `BasedSpan` that requires the ST monad to retrieve the baseSpan
class (forall x y. Span (s x y)) => StateBasedSpan s where
    -- | `baseSpan` inside `ST`
    baseSpanST :: s x y -> ST x (s x y)
    -- | `baseSpanOff` inside `ST`
    baseSpanOffST :: s x y -> ST x (s x y)


-- * Util methods

-- | `compare` on unlifted `Int#`
cmp# :: Int# -> Int# -> Ordering
cmp# x y
    | x `lt#` y = LT
    | x `gt#` y = GT
    | True = EQ

eq# :: Int# -> Int# -> Bool
eq# x y = isTrue# (x ==# y)

lt# :: Int# -> Int# -> Bool
lt# x y = isTrue# (x <# y)

leq# :: Int# -> Int# -> Bool
leq# x y = isTrue# (x <=# y)

gt# :: Int# -> Int# -> Bool
gt# x y = isTrue# (x ># y)

geq# :: Int# -> Int# -> Bool
geq# x y = isTrue# (x >=# y)

inc# :: Int# -> Int#
inc# x = x +# 1#

max# :: Int# -> Int# -> Int#
max# x y = if isTrue# (x ># y) then x else y

min# :: Int# -> Int# -> Int#
min# x y = if isTrue# (x <# y) then x else y

_bounds :: Int# -> Int# -> Int# -> Int# -> (# Int#, Int# #)
_bounds o n p m =
    let lo = min# o p
        hi = max# (o +# n) (p +# m)
     in (# lo, hi -# lo #)

-- | Generic helper for `extends`. Takes left extension, right extension, cur offset, cur length, total capacity
_extends :: Int# -> Int# -> Int# -> Int# -> Int# -> (# Int#, Int# #)
_extends n m o l z =
    if n `geq#` 0# && m `geq#` 0# && n `leq#` o && (n +# m +# o +# l) `leq#` z
        then (# o -# n, l +# n +# m #)
        else (# -1#, -1# #)

-- | Generic helper for `isSliceOf` that takes (offset, length) pairs
_isSliceOf :: Int# -> Int# -> Int# -> Int# -> Maybe Int
_isSliceOf o l o' l' =
    if isTrue# (o >=# o') && isTrue# (o +# l <=# o' +# l')
        then Just (I# (o -# o'))
        else Nothing

{- | Generic helper or `overlap` on (offset, length) pairs.
 Returns (-1,-1) to signal that there is no overlap
-}
_overlap :: Int# -> Int# -> Int# -> Int# -> (# Int#, Int# #)
_overlap o l o' l' =
    let oR = max# o o'
        hR = min# (o +# l) (o' +# l')
     in if isTrue# (oR <=# hR)
            then (# oR, (hR -# oR) #)
            else (# -1#, -1# #)

{- | Bounds-checks a slicing operation
takes (offset, length) pairs, slice first, then array
-}
_slice :: Int# -> Int# -> Int# -> Int# -> Int#
_slice d n o l =
    if d `geq#` 0# && n `geq#` 0# && (d +# n) `leq#` l
        then o +# d
        else -1#

{- | tail-recursive for loop with foldl-operator
 Bounds given by low (inclusive) and high (exclusive) value
-}
for :: forall a b. (b -> a -> b) -> (Int# -> a) -> Int# -> Int# -> b -> b
for op at i0 hi = loop i0
  where
    loop :: Int# -> b -> b
    loop i b =
        if i `lt#` hi
            then loop (inc# i) (b `op` at i)
            else b

{- | tail-recursive reverse for loop with foldr-operator
 Bounds given by low (inclusive) and high (exclusive) value
-}
forr :: forall a b. (a -> b -> b) -> (Int# -> a) -> Int# -> Int# -> b -> b
forr op at lo hi = loop (hi -# 1#)
  where
    loop :: Int# -> b -> b
    loop i b =
        if i `geq#` lo
            then loop (i -# 1#) (at i `op` b)
            else b

-- | Fast implementation for @~ if @ is O(1)
atConstRev :: (Span xs, AtConst xs, Index xs ~ Int) => xs -> Int -> Maybe (IxValue xs)
atConstRev xs i = xs @ (size xs - i - 1)
