module Data.Internal.MutSpan (MutSpan (), snocMutSpan, newMutSpan, unsafeFreezeSpan) where

import Data.Internal.ISpan
import Data.Internal.Span
import GHC.Base
import GHC.Exts

data MutSpan s (x :: TYPE (BoxedRep l)) = MS Int# Int# (SmallMutableArray# s x)

snocMutSpan :: MutSpan s x -> x -> State# s -> (# State# s, MutSpan s x #)
snocMutSpan xs x s =
    let !(# s1, MS c n a #) = ensure# 1# xs s
        !s2 = writeSmallArray# a n x s1
     in (# s2, MS c (inc# n) a #)
  where
    ensure# :: Int# -> MutSpan s b -> State# s -> (# State# s, MutSpan s b #)
    ensure# k x@(MS c n buf) s =
        let c' = newCap c
         in if c' `gt#` c
                then
                    let !(# s', buf' #) = resizeSmallMutableArray# buf c' undefined s in (# s', MS c' n buf' #)
                else
                    (# s, x #)
      where
        newCap a = if (n +# k) `gt#` a then newCap (2# *# a) else a

newMutSpan :: State# s -> (# State# s, MutSpan s x #)
newMutSpan s =
    let
        c0 = 64#
        !(# s1, xs #) = newSmallArray# c0 undefined s
        st = MS c0 0# xs
     in
        (# s1, st #)

unsafeFreezeSpan :: MutSpan s x -> State# s -> (# State# s, Span x #)
unsafeFreezeSpan (MS _ n arr) s =
    let
        s1 = shrinkSmallMutableArray# arr n s
        !(# s2, zs #) = unsafeFreezeSmallArray# arr s1
     in
        (# s2, fromSArray# zs #)
