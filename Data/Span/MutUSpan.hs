module Data.Span.MutUSpan (
    MutUSpan(),
    memcpy, memcpy#
) where

import Data.Span.Internal
import Turbo.RootPrelude
import GHC.Exts (copyMutableByteArray#)
import Data.Primitive (Prim(..))

memcpy :: MutUSpan s a -> MutUSpan s a -> ST s Int 
memcpy t f = ST \s -> let !(# s', z #) = memcpy# t f s in (# s', I# z #)

memcpy# :: MutUSpan s a -> MutUSpan s a -> State# s -> (# State# s, Int# #)
memcpy# (MutUSpan i n dst) (MutUSpan j m src) s0 = let
    !z = min# n m
    !s1 = copyMutableByteArray# src j dst i z s0
 in
    (# s1, z #)

capacity :: (Prim a) => Proxy a -> MutableByteArray# s -> State# s -> (# State# s, Int# #)
capacity p bs = _

instance Span (MutUSpan s a) where
    bounds = _
    extends = _
    isSliceOf = _
    overlap = _
    ptrCmp = _
    size = _
    slice = _
    sliceEnd = _
    takes = _
    trims = _

instance StateBasedSpan MutUSpan where
    