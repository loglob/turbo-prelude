module Data.Span.MutSpan (MutSpan (), newMutSpan, newMutSpan#, snocMutSpan, snocMutSpan#, unsafeFreezeSpan, unsafeFreezeSpan#) where

import Data.Span.Internal
import GHC.Err (undefined)
import GHC.Exts (RuntimeRep (BoxedRep), unsafeThawArray#, unsafeThawSmallArray#, andI#)
import Turbo.Prelude

instance Span (MutSpan s a) where
    baseSpanOff (MutSpan o _ a) = (_, I# o)
    extends = _
    bounds = _
    isSliceOf = _
    size (MutSpan _ n _) = I# n
    overlap = _
    ptrCmp = _
    slice = _

-- | Copies the contents of a mutable span into another mutable span
memcpy :: MutSpan s x -> MutSpan s x -> ST s ()
memcpy = _

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
    resizeMutSpan# cur newSize s1

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

-- | Trims this baseSpan to the current span's dimensions, then freezes the result.
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
