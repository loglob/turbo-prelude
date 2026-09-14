module Data.Internal.USpan (
    USpan (),
    fromBytes, fromBytes#,
    fromList,
    memcpy, memcpy#
) where

import Data.Foldable qualified
import Data.Primitive
import Data.Span.Internal
import GHC.Base
import Turbo.Internal.Classes
import Turbo.Prelude hiding (for)

capacity :: (Prim a) => Proxy a -> ByteArray# -> Int#
capacity p bs = sizeofByteArray# bs `divInt#` sizeOfType# p

ptrEq :: ByteArray# -> ByteArray# -> Bool
ptrEq x y = isTrue# (sameByteArray# x y)

-- looks almost exactly like the one for ArraySpan, but just different enough to not be generalizable further
instance Span (USpan a) where
    bounds :: USpan a -> USpan a -> Maybe (USpan a)
    bounds (USpan i n xs) (USpan j m ys) 
        | ptrEq xs ys = let !(# q, k #) = _bounds (# i, n #) (# j, m #) in Just (USpan q k xs)
        | otherwise   = Nothing

    extends :: Int -> Int -> USpan a -> USpan a
    extends (I# l) (I# r) (USpan i n arr) = case _extends l r (# i, n #) (capacity (Proxy :: Proxy a) arr) of
        (# _ | #)          -> error "extends indices out of range"
        (# | (# o, l #) #) -> USpan o l arr

    isSliceOf :: USpan a -> USpan a -> Maybe Int
    isSliceOf (USpan i n xs) (USpan j m ys) 
        | ptrEq xs ys = _isSliceOf (# i, n #) (# j, m #)
        | otherwise   = Nothing

    size :: USpan a -> Int
    size (USpan _ n _) = I# n

    overlap :: USpan a -> USpan a -> Maybe (USpan a)
    overlap (USpan i n xs) (USpan j m ys) 
        | ptrEq xs ys = case _overlap (# i, n #) (# j, m #) of
            !(# _ | #) -> Nothing
            !(# | (# o, l #) #) -> Just (USpan o l xs)
        | otherwise   = Nothing

    ptrCmp :: USpan a -> USpan a -> Maybe Ordering
    ptrCmp (USpan o _ xs) (USpan p _ ys) 
        | ptrEq xs ys = Just (cmp# o p)
        | otherwise   = Nothing

    slice :: Int -> Int -> USpan a -> USpan a
    slice (I# i) (I# n) (USpan j m xs) = case _slice (# i, n #) (# j, m #) of
        (# _ | #) -> error "slice index out of range"
        (# | o #) -> USpan o n xs

instance BasedSpan (USpan a) where
    baseSpanOff :: USpan a -> (USpan a, Int)
    baseSpanOff (USpan o _ arr) = (fromBytes# arr, I# o)

type instance IxValue (USpan a) = a

type instance Index (USpan a) = Int

instance AtConst (USpan a) where
    (@) :: USpan a -> Int -> Maybe a
    (USpan o l xs) @ (I# i) =
        if i `geq#` 0# && i `lt#` l
            then Just (indexByteArray# xs (i +# o))
            else Nothing

instance AtConstRev (USpan a) a where
    (@~) :: USpan a -> Int -> Maybe a
    (@~) = atConstRev

instance Foldable USpan where
    foldl :: (b -> a -> b) -> b -> USpan a -> b
    foldl f b0 (USpan o l bs) = for f (indexByteArray# bs) o (o +# l) b0
    foldr :: (a -> b -> b) -> b -> USpan a -> b
    foldr f b0 (USpan o l bs) = forr f (indexByteArray# bs) o (o +# l) b0
    null (USpan _ l _) = l `leq#` 0#
    length (USpan _ l _) = I# l

instance (Show a) => Show (USpan a) where
    showsPrec n = showsPrec n . toList

instance Uncons (USpan a) a where
    uncons :: USpan a -> Maybe (a, USpan a)
    uncons (USpan _ 0# _) = Nothing
    uncons (USpan o n xs) = Just (indexByteArray# xs o, USpan (o +# 1#) (n -# 1#) xs)

instance Unsnoc (USpan a) a where
    unsnoc :: USpan a -> Maybe (USpan a, a)
    unsnoc (USpan _ 0# _) = Nothing
    unsnoc (USpan o n xs) = Just (USpan o (n -# 1#) xs, indexByteArray# xs (o +# n -# 1#))

fromBytes :: forall a. (Prim a) => ByteArray -> USpan a
fromBytes (ByteArray b) = fromBytes# b

fromBytes# :: forall a. (Prim a) => ByteArray# -> USpan a
fromBytes# bs = USpan 0# (capacity (Proxy :: Proxy a) bs) bs

-- | Allocates a list of primitives to a byte array, then creates an equivalent unboxed span
fromList :: forall a. (Prim a) => [a] -> USpan a
fromList = \xs -> runST (ST (f xs))
  where
    f :: [a] -> State# s -> (# State# s, USpan a #)
    f xs s =
        let n = 128#
            !(# s1, mut #) = newByteArray# (n *# siz) s
            !(# s2, cop, l #) = copy s1 n mut 0# xs
         in (# s2, USpan 0# l cop #)
    siz = sizeOfType# (Proxy :: Proxy a)
    copy :: State# s -> Int# -> MutableByteArray# s -> Int# -> [a] -> (# State# s, ByteArray#, Int# #)
    copy s _ arr l [] =
        let !(# s1, arr1 #) = resizeMutableByteArray# arr (l *# siz) s
            !(# s2, arr2 #) = unsafeFreezeByteArray# arr1 s1
         in (# s2, arr2, l #)
    copy s c arr l xs
        | c `eq#` l =
            let l' = 2# *# l
                !(# s', arr' #) = resizeMutableByteArray# arr (l' *# siz) s
             in copy s' l' arr' l xs
    copy s c arr l (x : xs) =
        let s' = writeByteArray# arr l x s
         in copy s' c arr (inc# l) xs

-- | Copies from an unboxed span into a mutable unboxed span
--   If spans have different size, only copies until either the destination is filled or the source is exhausted.
--   returns the amount of bytes copied
memcpy :: MutUSpan s a -> USpan a -> ST s Int
memcpy t f = ST \s -> let !(# s', z #) = memcpy# t f s in (# s', I# z #)

memcpy# :: MutUSpan s a -> USpan a -> State# s -> (# State# s, Int# #)
memcpy# (MutUSpan i n dest) (USpan j m src) s0 = let
    !z = min# n m
    !s1 = copyByteArray# src j dest i z s0
 in
    (# s1, z #)
