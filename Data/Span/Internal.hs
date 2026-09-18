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
class Span span where
    -- | Computes the smallest span that contains both input spans
    --   Returns `Nothing` if they are part of different base spans
    bounds :: span -> span -> Maybe span

    -- | Extends a span to the left and right by the given number.
    --   Partial if indices are out-of-bounds.
    extends :: Int -> Int -> span -> span

    -- | Undoes `slice`, returning its first arguments
    isSliceOf :: span -> span -> Maybe Int

    -- | Computes the largest span that is a slice of both given spans.
    --   Returns `Nothing` when they don't overlap.
    --   Returns an empty span if the spans are exactly next to another.
    overlap :: span -> span -> Maybe span

    -- | Compares the underlying pointers of two spans
    --   Returns `Nothing` if the spans point into different arrays,
    --   compares the starting address of the spans otherwise.
    ptrCmp :: span -> span -> Maybe Ordering

    -- | The length of a span
    size :: span -> Int

    -- | Creates a sub-span from an offset and a length
    --   Partial if indices are out of bounds
    slice :: Int -> Int -> span -> span
    slice n m = takes m . trims n 0

    -- | Like `slice`, but indexes from the end of the span rather than the start.
    --    Partial if indices are out of bounds.
    --
    --    (!) The last element is at index 1
    sliceEnd :: Int -> Int -> span -> span
    sliceEnd n m s = slice (size s - n) m s

    -- | Returns only the $1 leftmost elements of $2
    takes :: Int -> span -> span
    takes = slice 0

    -- | Trims the $1 leftmost and $2 rightmost elements of $3
    trims :: Int -> Int -> span -> span
    trims l r s =
        let z = size s
         in if l < 0 || r < 0 || l + r > z
                then error "trims indices out of range"
                else slice l (size s - l - r) s

    {-# MINIMAL (extends, bounds, isSliceOf, size, overlap, ptrCmp, (slice | (trims, takes))) #-}

-- | A span that may be mutated in-place
class Span span => MutableSpan span s a | span -> s, span -> a where
    read# :: span -> Int# -> State# s -> (# State# s, a #)
    read# xs i = doST (read xs (I# i))

    -- | Reads from the span at the given index
    --
    --   Partial if index is outside the span bounds
    read :: span -> Int -> ST s a
    read xs (I# i) = ST (read# xs i)

    write# :: span -> Int# -> a -> State# s -> State# s
    write# xs i a = doST' (write xs (I# i) a)

    -- | Writes to the span at given index
    --
    --   Partial if index is outside the span bounds
    write :: span -> Int -> a -> ST s ()
    write xs (I# i) a = ST \s -> (# write# xs i a s, () #)

    populate# :: span -> (Int# -> State# s -> (# State# s, a #)) -> State# s -> State# s
    populate# xs get = loop 0#
     where
        !(I# n) = size xs
        loop o s | o `geq#` n = s
        loop o s = let
            !(# s1, x #) = get o s
            !s2 = write# xs o x s1
         in
            loop (inc# o) s2

    -- | Overwrites this entire span by mapping each position to a new value
    populate :: span -> (Int -> ST s a) -> ST s ()
    populate xs f = ST \s -> (# populate# xs (\i s' -> let !(ST g) = f (I# i) in g s') s, () #)

    memmove# :: span -> span -> State# s -> (# State# s, Int# #)
    memmove# xs ys s = let !(# s', I# n #) = doST (memmove xs ys) s in (# s', n #)

    -- | Copies from one span to another, or within the same span.
    --   Safe for overlapping regions of the same span.
    memmove :: span -> span -> ST s Int 
    memmove t f = ST \s -> let !(# s', z #) = memmove# t f s in (# s', I# z #)

    -- | Creates an independent copy of the current state of a mutable span.
    --   Only copies the addressable region of the span, not its entire underlying storage.
    copy :: span -> ST s span
    copy src = do
        tmp <- malloc (size src)
        _ <- memmove tmp src

        return tmp

    -- malloc and calloc are implemented as a big circle so that any one can be used to implement all
    -- however, if the underlying primitive is a calloc, calloc# should be preferred over calloc for performance

    calloc# :: Int# -> a -> State# s -> (# State# s, span #)
    calloc# n a s0 = let
        !(# s1, buf #) = malloc# n s0
        !s2 = populate# buf (\_ s -> (# s, a #)) s1
     in
        (# s2, buf #)

    malloc# :: Int# -> State# s -> (# State# s, span #)
    malloc# n = doST (malloc (I# n)) 

    -- | Creates a new mutable span with the given size
    --   Does not initialize the contained memory (or sets it to undefined)
    malloc :: Int -> ST s span
    malloc n = calloc n undefined

    -- | Creates a new mutable span with the given size, filled with the given default value.
    --
    -- To implement MutableSpan with a calloc-like primitive, also implement calloc# instead for better performance
    calloc :: Int -> a -> ST s span
    calloc (I# n) a = ST (calloc# n a)

    -- | `baseSpan` inside `ST` (all mutable spans are based)
    baseSpanST :: span -> ST s span
    baseSpanST x = fmap fst (baseSpanOffST x)

    -- | `baseSpanOff` inside `ST`
    baseSpanOffST :: span -> ST s (span, Int)

    {-# MINIMAL ((read | read#), (write | write#), (memmove | memmove#), (calloc# | malloc | malloc# | calloc), baseSpanOffST) #-}


-- *** BasedSpan

-- | A span that can be traced back to the baseSpan that contains it
class Span span => BasedSpan span where
    -- | A span of the entire array the input span slices
    baseSpan :: span -> span
    baseSpan = fst . baseSpanOff

    -- | Like `baseSpan` but also returns the starting offset of the input span
    baseSpanOff :: span -> (span, Int)
    baseSpanOff x =
        let
            b = baseSpan x
            o = x `isSliceOf` b
         in
            (b, o ?! error "Span violated slice law")

-- *** memcpy

-- | Indicates that two span types (one mutable, one not) have compatible memory layout that allows for direct copying
class (MutableSpan dst s a, Span src) => Copyable dst s a src | dst -> src, dst -> s, dst -> a, src -> a where
    memcpy# :: dst -> src -> State# s -> State# s
    memcpy# to fr = doST' (memcpy to fr)

    -- | Copies data from an immutable span into a mutable span
    memcpy :: dst -> src -> ST s ()
    memcpy to fr = ST \s -> (# memcpy# to fr s, () #) 

    -- | Copies the current contents of a mutable span into an equivalent immutable span
    freezeCopy :: dst -> ST s src

    -- | Creates a mutable independent copy of an immutable span
    mutableCopy :: src -> ST s dst
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
