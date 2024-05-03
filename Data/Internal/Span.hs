module Data.Internal.Span (
    Span (),
    fromArray,
    fromArray#,
    fromSArray#,
    fromList,
) where

import Data.Internal.ISpan
import GHC.Arr (Array (..))
import GHC.Base
import GHC.Exts (resizeSmallMutableArray#)
import GHC.ST
import Turbo.Prelude hiding (for)

{- | Permit either small or regular arrays
Differences should be negligible because they are immutable
(I think they are only separate types because they could be thawed again)
-}
type GenArray# (a :: TYPE (BoxedRep l)) = (# Array# a | SmallArray# a #)

at# :: GenArray# a -> Int# -> a
at# (# a | #) i = let !(# x #) = (indexArray# a i) in x
at# (# | a #) i = let !(# x #) = (indexSmallArray# a i) in x

samePtr :: GenArray# a -> GenArray# a -> Bool
samePtr (# x | #) (# y | #) = isTrue# (unsafePtrEquality# x y)
samePtr (# | x #) (# | y #) = isTrue# (unsafePtrEquality# x y)
samePtr _ _ = False

baseSpan# :: GenArray# a -> Span a
baseSpan# (# a | #) = fromArray# a
baseSpan# (# | a #) = fromSArray# a

{- | A segment of an immutable array
 Permits pointer-equality and comparison, rather than structural equality
-}
data Span (a :: TYPE (BoxedRep l)) = Span Int# Int# (GenArray# a)

instance ISpan (Span a) where
    baseSpan :: Span a -> Span a
    baseSpan (Span _ _ xs) = baseSpan# xs

    extends :: Int -> Int -> Span a -> Span a
    extends (I# l) (I# r) (Span o n xs) = slice (I# (o -# l)) (I# (n +# r)) (baseSpan# xs)

    isSliceOf :: Span a -> Span a -> Maybe Int
    isSliceOf (Span o l xs) (Span o' l' ys) = if samePtr xs ys then _isSliceOf o l o' l' else Nothing

    size :: Span a -> Int
    size (Span _ l _) = I# l

    overlap :: Span a -> Span a -> Maybe (Span a)
    overlap (Span o l xs) (Span o' l' ys) =
        if samePtr xs ys
            then case _overlap o l o' l' of
                (# -1#, _ #) -> Nothing
                (# oR, lR #) -> Just (Span oR lR xs)
            else Nothing

    bounds :: Span a -> Span a -> Maybe (Span a)
    bounds (Span o n xs) (Span p m ys) =
        if samePtr xs ys
            then let !(# q, k #) = _bounds o n p m in Just (Span q k xs)
            else Nothing

    ptrCmp :: Span a -> Span a -> Maybe Ordering
    ptrCmp (Span o _ xs) (Span p _ ys) = case samePtr xs ys of
        True -> Just (cmp# o p)
        False -> Nothing

    slice :: Int -> Int -> Span a -> Span a
    slice (I# d) (I# n) (Span o l xs) = case _slice d n o l of
        -1# -> error "slice indices out of bounds"
        oR -> Span oR n xs

type instance IxValue (Span a) = a

type instance Index (Span a) = Int

instance AtConst (Span a) where
    (@) :: Span a -> Int -> Maybe a
    (Span o l xs) @ (I# i) =
        if i `geq#` 0# && i `lt#` l
            then Just (at# xs (o +# i))
            else Nothing

instance AtConstRev (Span a) a where
    (@~) :: Span a -> Int -> Maybe a
    (@~) = atConstRev

instance Foldable Span where
    foldl :: forall a b. (b -> a -> b) -> b -> Span a -> b
    foldl f b0 (Span o l xs) = for f (at# xs) o (o +# l) b0
    foldr :: forall a b. (a -> b -> b) -> b -> Span a -> b
    foldr f b0 (Span o l xs) = forr f (at# xs) o (o +# l) b0
    null (Span _ l _) = l `eq#` 0#
    length (Span _ l _) = I# l

instance (Show a) => Show (Span a) where
    showsPrec p xs = showsPrec p (toList xs)

instance Uncons (Span a) a where
    uncons :: Span a -> Maybe (a, Span a)
    uncons (Span _ 0# _) = Nothing
    uncons (Span o n xs) = Just (xs `at#` o, Span (o +# 1#) (n -# 1#) xs)

{- | Aliases an array as a span.
 Discards index types completely, rebasing the array to 0.
-}
fromArray :: Array i a -> Span a
fromArray (Array _ _ (I# n) xs) = Span 0# n (# xs | #)

-- | Aliases an Array# as a span
fromArray# :: Array# a -> Span a
fromArray# a = Span 0# (sizeofArray# a) (# a | #)

-- | Aliases a SmallArray# as a span
fromSArray# :: SmallArray# a -> Span a
fromSArray# a = Span 0# (sizeofSmallArray# a) (# | a #)

-- | Allocates a list to a small array, then creates an equivalent span
fromList :: forall a. [a] -> Span a
fromList = \xs -> runST (ST (f xs))
  where
    f :: [a] -> State# s -> (# State# s, Span a #)
    f xs s =
        let siz = 128#
         in let !(# s1, mut #) = newSmallArray# siz (undefined :: a) s
             in let !(# s2, cop #) = copy s1 siz mut 0# xs
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
         in let !(# s2, arr' #) = unsafeFreezeSmallArray# arr s1
             in (# s2, arr' #)
    copy s c arr l xs
        | c `eq#` l =
            let l' = 2# *# l
             in let !(# s', arr' #) = resizeSmallMutableArray# arr l' undefined s
                 in copy s' l' arr' l xs
    copy s c arr l (x : xs) =
        let s' = writeSmallArray# arr l x s
         in copy s' c arr (inc# l) xs
