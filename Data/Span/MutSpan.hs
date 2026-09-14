module Data.Span.MutSpan (
    MutSpan (),
    newMutSpan, newMutSpan#,
    populate, populate#,
    snocMutSpan, snocMutSpan#,
    unsafeFreezeSpan, unsafeFreezeSpan#,
    set, set#,
    get, get#,
    memcpy, memcpy#,
    memdup,
    freezeCopy
 ) where

import Data.Span.Internal
import GHC.Err (undefined)
import GHC.Exts (andI#, copySmallMutableArray#, copyMutableArray#, sameMutableArray#, sameSmallMutableArray#)
import Turbo.RootPrelude hiding (set)
import GHC.Base (error)

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

instance StateBasedSpan MutSpan where
    baseSpanOffST (MutSpan o _ g) = ST \s0 -> let
        !(# s1, z #) = case g of
            (# a | #) -> (# s0, sizeofMutableArray# a #)
            (# | a #) -> getSizeofSmallMutableArray# a s0
     in
        (# s1, (MutSpan 0# z g, I# o) #)

set# :: MutSpan s x -> Int# -> x -> State# s -> State# s
set# (MutSpan i n g) j x s
    | j `geq#` n = error "Index out of bounds"
    | otherwise  = case g of
        (# a | #) -> writeArray# a (i +# j) x s
        (# | a #) -> writeSmallArray# a (i +# j) x s

-- | Writing operation for mutable span
set :: MutSpan s x -> Int -> x -> ST s ()
set m (I# i) x = ST \s -> (# set# m i x s, () #)

get# :: MutSpan s x -> Int# -> State# s -> (# State# s, x #)
get# (MutSpan i n g) j s 
    | j `geq#` n = error "Index out of bounds"
    | otherwise  = case g of
        (# a | #) -> readArray# a (i +# j) s
        (# | a #) -> readSmallArray# a (i +# j) s

-- | Indexing operation for mutable span
get :: MutSpan s x -> Int -> ST s x
get m (I# i) = ST (get# m i)

populate# :: MutSpan s x -> (Int# -> State# s -> (# State# s, x #)) -> State# s -> State# s
populate# m@(MutSpan _ n _) get = loop 0#
 where
    loop o s | o `geq#` n = s
    loop o s = let
        !(# s1, x #) = get o s
        !s2 = set# m o x s1
     in
        loop (inc# o) s2

-- | Populates a span with the results of the given stateful computation
--  $1 - Span to fill
--  $2 - Function that determines values to place at each index. Indices are relative to span.
populate :: MutSpan s x -> (Int -> ST s x) -> ST s ()
populate m f = ST \s -> (# populate# m (\i s' -> let !(ST g) = f (I# i) in g s') s, () #)

-- | Copies the contents of a mutable span into another mutable span
--   $1 - Destination to copy into
--   $2 - Source to copy from
memcpy# :: MutSpan s x -> MutSpan s x -> State# s -> (# State# s, Int# #)
memcpy# (MutSpan i n dst) r@(MutSpan j m src) s = let
    !z = min# n m
    !s' = case (# dst, src #) of
        -- first check for primitive copy
        (# (# t | #), (# f | #) #) -> copyMutableArray# f i t j n s
        (# (# | t #), (# | f #) #) -> copySmallMutableArray# f i t j n s
        -- fall back to slow copy (need to reconstruct left arg so that size matches)
        _ -> populate# (MutSpan i z dst) (get# r) s
 in
    (# s', z #)

memcpy :: MutSpan s x -> MutSpan s x -> ST s Int
memcpy dst src = ST \s -> let !(# s', z #) = memcpy# dst src s in (# s', I# z #)

-- | Allocates an independent copy of a span. Only copies the addressable portion of the underlying array.
memdup :: MutSpan s x -> ST s (MutSpan s x)
memdup src@(MutSpan _ n arr) = do
    new <- ST \s -> case arr of
        (# _ | #) -> let !(# s', arr' #) = newArray# n undefined s      in (# s', MutSpan 0# n (# arr' | #) #)
        (# | _ #) -> let !(# s', arr' #) = newSmallArray# n undefined s in (# s', MutSpan 0# n (# | arr' #) #)
    _ <- memcpy new src
    return new

-- | Creates a frozen copy of this span. Only copies the addressable portion of the underlying array.
freezeCopy :: MutSpan s x -> ST s (ArraySpan x)
freezeCopy m = memdup m >>= unsafeFreezeSpan

-- | Trims the underlying array to represent precisely the given slice of it.
--   Either resizes the underlying array directly, or allocates a new array.
--   (!) Any other slice that aliases the same array must not be used afterwards
unsafeCompact :: MutSpan s x -> ST s (MutSpan s x)
unsafeCompact (MutSpan i n g) = ST \s0 -> let
    (# s1, _ #) = _
 in
    _

-- | Ensures the array underlying a span has at least a specified minimum capacity beyond the bounds of the array
--   May mutate the underlying array, allocate a new one, or leave it unchanged.
--   In case the array is mutated, NO other slice that aliases the same array must be used afterward.
ensureCapacity# :: MutSpan s x -> Int# -> State# s -> (# State# s, MutSpan s x #)
ensureCapacity# cur@(MutSpan i n g) m s0 = let
    !want = i +# n +# m
    !(# s1, have #) = case g of
        (# a | #) -> (# s0, sizeofMutableArray# a #)
        (# | a #) -> getSizeofSmallMutableArray# a s0
    !diff = want -# have
 in if diff `leq#` 0# then (# s1, cur #) else let
    -- power of 2
    !grain = 256#
    !rest = diff `andI#` (grain -# 1#)
    -- round up to multiple of grain size
    !padding = if rest `gt#` 0# then grain -# rest else 0#
    !newSize = want +# padding
 in 
    _

-- | Appends to the right of a mutable span
-- (!) the returned slice 
snocMutSpan :: MutSpan s x -> x -> ST s (MutSpan s x)
snocMutSpan (MutSpan o n g) x = do
    _

snocMutSpan# :: MutSpan s x -> x -> State# s -> (# State# s, MutSpan s x #)
snocMutSpan# xs x s =
    let !(# s1, MutSpan c n a #) = ensure# 1# xs s
        !s2 = writeSmallArray# a n x s1
     in (# s2, MutSpan c (inc# n) a #)
  where
    ensure# :: Int# -> MutSpan s b -> State# s -> (# State# s, MutSpan s b #)
    ensure# k x@(MutSpan c n buf) s =
        let c' = newCap c
         in if c' `gt#` c
                then
                    let !(# s', buf' #) = resizeSmallMutableArray# buf c' undefined s in (# s', MutSpan c' n buf' #)
                else
                    (# s, x #)
      where
        newCap a = if (n +# k) `gt#` a then newCap (2# *# a) else a

newMutSpan :: ST s (MutSpan s x)
newMutSpan = ST newMutSpan#

newMutSpan# :: forall s x. State# s -> (# State# s, MutSpan s x #)
newMutSpan# s =
    let
        c0 = 64#
        !(# s1, xs #) = newSmallArray# c0 undefined s
        st = MutSpan c0 0# (# | xs #)
     in
        (# s1, st #)

-- | Freezes the array underlying a span.
--   (!) Any references to the mutable array must not be used afterwards.
unsafeFreezeSpan :: MutSpan s x -> ST s (ArraySpan x)
unsafeFreezeSpan x = ST (unsafeFreezeSpan# x)

unsafeFreezeSpan# :: MutSpan s x -> State# s -> (# State# s, ArraySpan x #)
unsafeFreezeSpan# (MutSpan o n g) s = case g of
    (# a | #) -> let
        !(# s', a' #) = unsafeFreezeArray# a s
     in
        (# s', ArraySpan o n (# a' | #) #)
    (# | a #) -> let
        !(# s', a' #) = unsafeFreezeSmallArray# a s
     in
        (# s', ArraySpan o n (# | a' #) #)
