module Data.Span.ArraySpan (
    ArraySpan (..),
    fromArray,
    fromArray#,
    fromSArray#,
    fromList,
    GenArray#
) where

import Data.Foldable qualified
import Data.Span.Internal
import GHC.Arr (Array (..))
import GHC.Err (error, undefined)
import GHC.Exts (RuntimeRep (..))
import GHC.ST
import Turbo.Internal.Classes
import Turbo.Prelude hiding (for)

at# :: GenArray# a -> Int# -> a
at# (# a | #) i = let !(# x #) = (indexArray# a i) in x
at# (# | a #) i = let !(# x #) = (indexSmallArray# a i) in x

samePtr :: GenArray# a -> GenArray# a -> Bool
samePtr (# x | #) (# y | #) = isTrue# (unsafePtrEquality# x y)
samePtr (# | x #) (# | y #) = isTrue# (unsafePtrEquality# x y)
samePtr _ _ = False

baseSpan# :: GenArray# a -> ArraySpan a
baseSpan# (# a | #) = fromArray# a
baseSpan# (# | a #) = fromSArray# a

instance Span (ArraySpan a) where
    baseSpanOff :: ArraySpan a -> (ArraySpan a, Int)
    baseSpanOff (ArraySpan o _ xs) = (baseSpan# xs, I# o)

    extends :: Int -> Int -> ArraySpan a -> ArraySpan a
    extends (I# l) (I# r) (ArraySpan o n xs) = slice (I# (o -# l)) (I# (n +# r)) (baseSpan# xs)

    isSliceOf :: ArraySpan a -> ArraySpan a -> Maybe Int
    isSliceOf (ArraySpan o l xs) (ArraySpan o' l' ys) = if samePtr xs ys then _isSliceOf o l o' l' else Nothing

    size :: ArraySpan a -> Int
    size (ArraySpan _ l _) = I# l

    overlap :: ArraySpan a -> ArraySpan a -> Maybe (ArraySpan a)
    overlap (ArraySpan o l xs) (ArraySpan o' l' ys) =
        if samePtr xs ys
            then case _overlap o l o' l' of
                (# -1#, _ #) -> Nothing
                (# oR, lR #) -> Just (ArraySpan oR lR xs)
            else Nothing

    bounds :: ArraySpan a -> ArraySpan a -> Maybe (ArraySpan a)
    bounds (ArraySpan o n xs) (ArraySpan p m ys) =
        if samePtr xs ys
            then let !(# q, k #) = _bounds o n p m in Just (ArraySpan q k xs)
            else Nothing

    ptrCmp :: ArraySpan a -> ArraySpan a -> Maybe Ordering
    ptrCmp (ArraySpan o _ xs) (ArraySpan p _ ys) = case samePtr xs ys of
        True -> Just (cmp# o p)
        False -> Nothing

    slice :: Int -> Int -> ArraySpan a -> ArraySpan a
    slice (I# d) (I# n) (ArraySpan o l xs) = case _slice d n o l of
        -1# -> error "slice indices out of bounds"
        oR -> ArraySpan oR n xs

type instance IxValue (ArraySpan a) = a

type instance Index (ArraySpan a) = Int

instance AtConst (ArraySpan a) where
    (@) :: ArraySpan a -> Int -> Maybe a
    (ArraySpan o l xs) @ (I# i) =
        if i `geq#` 0# && i `lt#` l
            then Just (at# xs (o +# i))
            else Nothing

instance AtConstRev (ArraySpan a) a where
    (@~) :: ArraySpan a -> Int -> Maybe a
    (@~) = atConstRev

instance Foldable ArraySpan where
    foldl :: (b -> a -> b) -> b -> ArraySpan a -> b
    foldl f b0 (ArraySpan o l xs) = for f (at# xs) o (o +# l) b0
    foldr :: (a -> b -> b) -> b -> ArraySpan a -> b
    foldr f b0 (ArraySpan o l xs) = forr f (at# xs) o (o +# l) b0
    null (ArraySpan _ l _) = l `eq#` 0#
    length (ArraySpan _ l _) = I# l

instance (Show a) => Show (ArraySpan a) where
    showsPrec p xs = showsPrec p (toList xs)

instance Uncons (ArraySpan a) a where
    uncons :: ArraySpan a -> Maybe (a, ArraySpan a)
    uncons (ArraySpan _ 0# _) = Nothing
    uncons (ArraySpan o n xs) = Just (xs `at#` o, ArraySpan (o +# 1#) (n -# 1#) xs)

instance Unsnoc (ArraySpan a) a where
    unsnoc :: ArraySpan a -> Maybe (ArraySpan a, a)
    unsnoc (ArraySpan _ 0# _) = Nothing
    unsnoc (ArraySpan o n xs) = Just (ArraySpan o (n -# 1#) xs, xs `at#` (o +# n -# 1#))

{- | Aliases an array as a span.
 Discards index types completely, rebasing the array to 0.
-}
fromArray :: Array i a -> ArraySpan a
fromArray (Array _ _ (I# n) xs) = ArraySpan 0# n (# xs | #)

-- | Aliases an Array# as a span
fromArray# :: Array# a -> ArraySpan a
fromArray# a = ArraySpan 0# (sizeofArray# a) (# a | #)

-- | Aliases a SmallArray# as a span
fromSArray# :: SmallArray# a -> ArraySpan a
fromSArray# a = ArraySpan 0# (sizeofSmallArray# a) (# | a #)

-- | Allocates a list to a small array, then creates an equivalent span
fromList :: [a] -> ArraySpan a
fromList = \xs -> runST (ST (f xs))
  where
    f :: [a] -> State# s -> (# State# s, ArraySpan a #)
    f xs s =
        let siz = 128#
            !(# s1, mut #) = newSmallArray# siz (undefined :: a) s
            !(# s2, cop #) = copy s1 siz mut 0# xs
         in (# s2, fromSArray# cop #)
    -- \| Copies a list into an array
    --   - $1: state thread
    --   - $2: Capacity of $3
    --   - $3: Current array
    --   - $4: Number of inserted entries
    --   - $5: List to copy
    --   Returns: ( state thread, finished array )
    copy :: State# s -> Int# -> SmallMutableArray# s a -> Int# -> [a] -> (# State# s, SmallArray# a #)
    copy s _ arr l [] =
        let s1 = shrinkSmallMutableArray# arr l s
            !(# s2, arr' #) = unsafeFreezeSmallArray# arr s1
         in (# s2, arr' #)
    copy s c arr l xs
        | c `eq#` l =
            let l' = 2# *# l
                !(# s', arr' #) = resizeSmallMutableArray# arr l' undefined s
             in copy s' l' arr' l xs
    copy s c arr l (x : xs) =
        let s' = writeSmallArray# arr l x s
         in copy s' c arr (inc# l) xs
