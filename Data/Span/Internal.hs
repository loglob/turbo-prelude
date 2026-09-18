{-# LANGUAGE QuantifiedConstraints #-}
module Data.Span.Internal where
import Data.Primitive
import GHC.Base
import Turbo.Internal.Classes
import Turbo.Operators ((?!))
import Turbo.RootPrelude
import Turbo.Extra (doST', doST)

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


class (forall x y. Span (s x y)) => MutableSpan s where
    read# :: s x y -> Int# -> State# x -> (# State# x, y #)
    read# xs i = doST (read xs (I# i))

    read :: s x y -> Int -> ST x y
    read xs (I# i) = ST (read# xs i)

    write# :: s x y -> Int# -> y -> State# x -> State# x
    write# xs i y = doST' (write xs (I# i) y)

    write :: s x y -> Int -> y -> ST x ()
    write xs (I# i) y = ST \s -> (# write# xs i y s, () #)

    populate# :: s x y -> (Int# -> State# x -> (# State# x, y #)) -> State# x -> State# x
    populate# xs get = loop 0#
     where
        !(I# n) = size xs
        loop o s | o `geq#` n = s
        loop o s = let
            !(# s1, x #) = get o s
            !s2 = write# xs o x s1
         in
            loop (inc# o) s2

    populate :: s x y -> (Int -> ST x y) -> ST x ()
    populate xs f = ST \s -> (# populate# xs (\i s' -> let !(ST g) = f (I# i) in g s') s, () #)

    memmove# :: s x y -> s x y -> State# x -> (# State# x, Int# #)
    memmove# xs ys s = let !(# s', I# n #) = doST (memmove xs ys) s in (# s', n #)

    memmove :: s x y -> s x y -> ST x Int 
    memmove t f = ST \s -> let !(# s', z #) = memmove# t f s in (# s', I# z #)

    -- | Creates an independent copy of the current state of a mutable span. Only copies the addressable region of the span, not its entire underlying storage.
    copy :: s x y -> ST x (s x y)
    copy src = do
        tmp <- calloc (size src) undefined
        _ <- memmove tmp src

        return tmp

    calloc# :: Int# -> y -> State# x -> (# State# x, s x y #)
    calloc# n y = doST (calloc (I# n) y)

    calloc :: Int -> y -> ST x (s x y)
    calloc (I# n) y = ST (calloc# n y)

    {-# MINIMAL ((read | read#), (write | write#), (memmove | memmove#), copy, (calloc | calloc#)) #-}


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
    baseSpanST x = fmap fst (baseSpanOffST x)

    -- | `baseSpanOff` inside `ST`
    baseSpanOffST :: s x y -> ST x (s x y, Int)

-- *** Memcpy

-- | Indicates that two span types (one mutable, one not) have compatible memory layout that allows for direct copying
class (MutableSpan dst, forall a. Span (src a)) => Copyable dst src where
    memcpy# :: dst s a -> src a -> State# s -> State# s
    memcpy# to fr = doST' (memcpy to fr)

    -- | Copies data from an immutable span into a mutable span
    memcpy :: dst s a -> src a -> ST s ()
    memcpy to fr = ST \s -> (# memcpy# to fr s, () #) 

    -- | Copies the current contents of a mutable span into an equivalent immutable span
    freezeCopy :: dst s a -> ST s (src a)

    -- | Creates a mutable independent copy of an immutable span
    mutableCopy :: src a -> ST s (dst s a)
    mutableCopy src = do
        mut <- calloc (size src) undefined
        memcpy mut src

        return mut


    {-# MINIMAL ((memcpy | memcpy#), freezeCopy) #-}

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

type OffsetLength = (# Int#, Int# #)

-- | Generic helper for `bounds`.
--  Takes two (offset, length) pairs then produces a third that is the smallest bound around both
_bounds :: OffsetLength -> OffsetLength -> OffsetLength
_bounds (# i, n #) (# j , m #) = let 
    lo = min# i j
    hi = max# (i +# n) (j +# m)
 in 
    (# lo, hi -# lo #)

type OptOffsetLength = (# (##) | OffsetLength #)

pattern OOB :: OptOffsetLength
pattern OOB <- !(# (##) | #) where
    OOB = (# (##) | #)

pattern InBounds :: OffsetLength -> OptOffsetLength
pattern InBounds x <- !(# | x #) where
    InBounds x = (# | x #)

-- | Weaker form of `_extends` without knowing the total capacity
--
-- $1 - left extension
-- $2 - right extension
-- $3 - current span dimensions
_extends' :: Int# -> Int# -> OffsetLength -> OptOffsetLength
_extends' l r (# i, n #)
    | l `lt#` 0# || r `lt#` 0# || l `gt#` i = OOB
    | otherwise                             = InBounds (# i -# l, n +# l +# r #)

-- | Generic helper for `extends`
--
-- $1 - left extension
-- $2 - right extension
-- $3 - current span dimensions
-- $4 - total capacity of underlying buffer
_extends :: Int# -> Int# -> OffsetLength -> Int# -> OptOffsetLength
_extends l r (# i, n #) z
    | (l +# r +# i +# n) `gt#` z = OOB
    | otherwise                = _extends' l r (# i, n #)

-- | Generic helper for `isSliceOf` that takes (offset, length) pairs
-- $1 - smaller slice candidate
-- $2 - larger base candidate
-- returns Index of slice within base span, if applicable
_isSliceOf :: OffsetLength -> OffsetLength -> Maybe Int
_isSliceOf (# i, n #) (# j, m #)
    | (i `geq#` j) && ((i +# n) `leq#` (j +# m)) = Just (I# (i -# j))
    | otherwise                                  = Nothing

-- | Generic helper or `overlap`
_overlap :: OffsetLength -> OffsetLength -> OptOffsetLength
_overlap (# i, n #) (# j, m #) =
    let oR = max# i j
        hR = min# (i +# n) (j +# m)
     in if oR `leq#` hR
        then InBounds (# oR, (hR -# oR) #)
        else OOB

{- | Bounds-checks a slicing operation

    $1 - relative offset+length
    $2 - current slice offset+length
    returns the total index of resulting slice, or unit on OOB
-}
_slice :: OffsetLength -> OffsetLength -> (# (##) | Int# #)
_slice (# i, n #) (# j, m #)
    | i `lt#` 0# || n `lt#` 0# || (i +# n) `gt#` m = (# (##) | #)
    | otherwise                                    = (# | i +# j #)

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
