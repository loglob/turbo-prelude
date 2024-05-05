{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Internal.VectorSpan where

import Data.Internal.ISpan
import Data.Primitive.Array (Array (..))
import Data.Vector qualified as V
import GHC.Err (error)
import Turbo.Prelude

instance ISpan (V.Vector v) where
    baseSpan :: V.Vector v -> V.Vector v
    baseSpan v = let (a, _, _) = V.toArraySlice v in V.unsafeFromArraySlice a 0 (length a)
    bounds :: V.Vector v -> V.Vector v -> Maybe (V.Vector v)
    bounds u v =
        let
            !(Array a, I# o, I# n) = V.toArraySlice u
            !(Array b, I# p, I# m) = V.toArraySlice v
         in
            case unsafePtrEquality# a b of
                1# -> let !(# q, k #) = _bounds o n p m in Just $ V.unsafeFromArraySlice (Array a) (I# q) (I# k)
                _ -> Nothing
    extends :: Int -> Int -> V.Vector v -> V.Vector v
    extends (I# n) (I# m) v =
        let
            !(a, I# o, I# l) = V.toArraySlice v
            !(I# z) = length a
         in
            case _extends n m o l z of
                (# -1#, _ #) -> error "extends indices out of range"
                (# p, k #) -> V.unsafeFromArraySlice a (I# p) (I# k)
    isSliceOf :: V.Vector v -> V.Vector v -> Maybe Int
    isSliceOf u v =
        let
            !(Array a, I# o, I# n) = V.toArraySlice u
            !(Array b, I# p, I# m) = V.toArraySlice v
         in
            case unsafePtrEquality# a b of
                1# -> _isSliceOf o n p m
                _ -> Nothing
    overlap :: V.Vector v -> V.Vector v -> Maybe (V.Vector v)
    overlap u v =
        let
            !(Array a, I# o, I# n) = V.toArraySlice u
            !(Array b, I# p, I# m) = V.toArraySlice v
         in
            case unsafePtrEquality# a b of
                1# -> case _overlap o n p m of
                    (# -1#, _ #) -> Nothing
                    (# q, k #) -> Just $ V.unsafeFromArraySlice (Array a) (I# q) (I# k)
                _ -> Nothing
    ptrCmp :: V.Vector v -> V.Vector v -> Maybe Ordering
    ptrCmp u v =
        let
            !(Array a, o, _) = V.toArraySlice u
            !(Array b, p, _) = V.toArraySlice v
         in
            case unsafePtrEquality# a b of
                1# -> Just (compare o p)
                _ -> Nothing
    size :: V.Vector v -> Int
    size = V.length
    slice :: Int -> Int -> V.Vector v -> V.Vector v
    slice = V.slice

instance AtConstRev (V.Vector v) v where
    (@~) = atConstRev
