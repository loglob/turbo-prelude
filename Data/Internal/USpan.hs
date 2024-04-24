module Data.Internal.USpan where

import Control.Extra
import Control.Lens (Index, IxValue)
import Data.Internal.ISpan
import Data.Primitive
import Data.Proxy
import GHC.Base
import GHC.ST
import Turbo.RootPrelude hiding (for)

{- | Segment of a byte array.
 Offers more compact and efficient representation, but doesn't support laziness.
-}
data USpan a where
    -- Use a GADT to bind the `Prim` constraint into the constructor, otherwise classes on USpan aren't doable

    -- | Offsets/length in numbers of elements, NOT bytes
    USpan :: (Prim a) => Int# -> Int# -> ByteArray# -> USpan a

capacity :: (Prim a) => Proxy a -> ByteArray# -> Int#
capacity p bs = sizeofByteArray# bs `divInt#` sizeOfType# p

-- looks almost exactly like the one for Span, but just different enough to not be generalizable further
instance ISpan (USpan a) where
    baseSpan :: USpan a -> USpan a
    baseSpan (USpan _ _ arr) = fromBytes# arr
    bounds :: USpan a -> USpan a -> Maybe (USpan a)
    bounds (USpan o n xs) (USpan p m ys) = case unsafePtrEquality# xs ys of
        1# -> let !(# q, k #) = _bounds o n p m in Just $ USpan q k xs
        _ -> Nothing
    extends :: Int -> Int -> USpan a -> USpan a
    extends (I# n) (I# m) (USpan o l arr) = case _extends n m o l (capacity (Proxy :: Proxy a) arr) of
        (# -1#, _ #) -> error "extends indices out of range"
        (# o', l' #) -> USpan o' l' arr
    isSliceOf :: USpan a -> USpan a -> Maybe Int
    isSliceOf (USpan o n xs) (USpan p m ys) = case unsafePtrEquality# xs ys of
        1# -> _isSliceOf o n p m
        _ -> Nothing
    size :: USpan a -> Int
    size (USpan _ n _) = I# n
    overlap :: USpan a -> USpan a -> Maybe (USpan a)
    overlap (USpan o n xs) (USpan p m ys) = case unsafePtrEquality# xs ys of
        1# -> case _overlap o n p m of
            (# -1#, _ #) -> Nothing
            (# oR, lR #) -> Just (USpan oR lR xs)
        _ -> Nothing
    ptrCmp :: USpan a -> USpan a -> Maybe Ordering
    ptrCmp (USpan o _ xs) (USpan p _ ys) = case unsafePtrEquality# xs ys of
        1# -> Just (cmp# o p)
        _ -> Nothing
    slice :: Int -> Int -> USpan a -> USpan a
    slice (I# d) (I# n) (USpan o l xs) = case _slice d n o l of
        -1# -> error "slice index out of range"
        oR -> USpan oR n xs

type instance IxValue (USpan a) = a

type instance Index (USpan a) = Int

instance AtConst (USpan a) where
    (@) :: USpan a -> Int -> Maybe a
    (USpan o l xs) @ (I# i) =
        if i `geq#` 0# && i `lt#` l
            then Just (indexByteArray# xs (i +# o))
            else Nothing

instance Foldable USpan where
    foldl :: (b -> a -> b) -> b -> USpan a -> b
    foldl f b0 (USpan o l bs) = for f (indexByteArray# bs) o (o +# l) b0
    foldr :: (a -> b -> b) -> b -> USpan a -> b
    foldr f b0 (USpan o l bs) = forr f (indexByteArray# bs) o (o +# l) b0
    null (USpan _ l _) = l `leq#` 0#
    length (USpan _ l _) = I# l

instance (Show a) => Show (USpan a) where
    showsPrec n = showsPrec n . toList

fromBytes# :: forall a. (Prim a) => ByteArray# -> USpan a
fromBytes# bs = USpan 0# (capacity (Proxy :: Proxy a) bs) bs

-- | Allocates a list of primitives to a byte array, then creates an equivalent unboxed span
fromListU :: forall a. (Prim a) => [a] -> USpan a
fromListU = \xs -> runST (ST (f xs))
  where
    f :: [a] -> State# s -> (# State# s, USpan a #)
    f xs s =
        let n = 128#
         in let !(# s1, mut #) = newByteArray# (n *# siz) s
             in let !(# s2, cop, l #) = copy s1 n mut 0# xs
                 in (# s2, USpan 0# l cop #)
    siz = sizeOfType# (Proxy :: Proxy a)
    copy :: State# s -> Int# -> MutableByteArray# s -> Int# -> [a] -> (# State# s, ByteArray#, Int# #)
    copy s _ arr l [] =
        let !(# s1, arr1 #) = resizeMutableByteArray# arr (l *# siz) s
         in let !(# s2, arr2 #) = unsafeFreezeByteArray# arr1 s1
             in (# s2, arr2, l #)
    copy s c arr l xs
        | c `eq#` l =
            let l' = 2# *# l
             in let !(# s', arr' #) = resizeMutableByteArray# arr (l' *# siz) s
                 in copy s' l' arr' l xs
    copy s c arr l (x : xs) =
        let s' = writeByteArray# arr l x s
         in copy s' c arr (inc# l) xs
