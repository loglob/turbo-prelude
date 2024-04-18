{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Data.Span (
    Span(),
    at,
    baseSpan,
    extends,
    fromArray,
    fromArray#,
    fromList,
    fromSArray#,
    isSliceOf,
    overlap,
    ptrCmp,
    slice,
    sliceEnd,
    trims,
) where
import GHC.Arr as A
import GHC.Base
import GHC.Exts (resizeSmallMutableArray#)
import GHC.ST
import Turbo.RootPrelude

-- * Internals
-- | Permit either small or regular arrays
--  Differences should be negligible because they are immutable
--  (I think they are only separate types because they could be thawed again) 
type GenArray# a = (# Array# a | SmallArray# a #)

at# :: GenArray# a -> Int# -> a
at# (# a | #) i = let !(# x #) = (indexArray# a i) in x
at# (# | a #) i = let !(# x #) = (indexSmallArray# a i) in x

samePtr :: GenArray# a -> GenArray# a -> Bool
samePtr (# x | #) (# y | #) = isTrue# (unsafePtrEquality# x y)
samePtr (# | x #) (# | y #) = isTrue# (unsafePtrEquality# x y)
samePtr _         _         = False

pos# :: Int# -> Bool
pos# x = isTrue# (x >=# 0#)

baseSpan# :: GenArray# a -> Span a
baseSpan# (# a | #) = fromArray# a
baseSpan# (# | a #) = fromSArray# a


-- * Type definition
-- ** Internal span class
class ISpan s where
    -- | A span of the entire array the input span slices
    baseSpan :: s -> s
    -- | Extends a span to the left and right by the given number.
    --   Partial if indices are out-of-bounds.
    extends :: Int -> Int -> s -> s
    -- | Undoes `slice`, returning its first arguments
    isSliceOf :: s -> s -> Maybe Int
    -- | The length of a span
    size :: s -> Int
    -- | Computes the span that is a slice of both given spans.
    --   Returns `Nothing` when they don't overlap.
    --   May return an empty span if the spans are exactly next to another.
    overlap :: s -> s -> Maybe s
    -- | Compares the underlying pointers of two spans
    --   Returns `Nothing` if the spans point into different arrays,
    --   compares the starting address of the spans otherwise.
    ptrCmp :: s -> s -> Maybe Ordering
    -- | Creates a sub-span from an offset and a length
    --   Partial if indices are out of bounds
    slice :: Int -> Int -> s -> s
    -- | Trims the $1 leftmost and $2 rightmost elements of $3
    trims :: Int -> Int -> s -> s

-- | Like `slice`, but indexes from the end of the span rather than the start.
--   Partial if indices are out of bounds.
--   
--   (!) The last element is at index 1
sliceEnd :: ISpan s => Int -> Int -> s -> s
sliceEnd n m s = slice (size s - n) m s

-- ** Array Span
-- | A segment of an immutable array
--   Permits pointer-equality and comparison, rather than structural equality
data Span a = Span Int# Int# (GenArray# a)

instance ISpan (Span a) where
    baseSpan :: Span a -> Span a
    baseSpan (Span _ _ xs) = baseSpan# xs

    extends :: Int -> Int -> Span a -> Span a
    extends (I# l) (I# r) (Span o n xs) = slice (I# (o -# l)) (I# (n +# r)) (baseSpan# xs)

    isSliceOf :: Span a -> Span a -> Maybe Int
    isSliceOf (Span o l xs) (Span o' l' ys) = if samePtr xs ys && isTrue# (o >=# o') && isTrue# (o +# l <=# o' +# l')
        then Just$ I# (o -# o')
        else Nothing
    
    size :: Span a -> Int
    size (Span _ l _) = I# l

    -- | Computes the span that is a slice of both given spans.
    --   Returns `Nothing` when they don't overlap.
    --   May return an empty span if the spans are exactly next to another.
    overlap :: Span a -> Span a -> Maybe (Span a)
    overlap (Span o l xs) (Span o' l' ys) = if samePtr xs ys
        then
            let oR = max# o o' in
            let hR = min# (o +# l) (o' +# l') in
            if isTrue# (oR <=# hR)
                then Just (Span oR (hR -# oR) xs)
                else Nothing
        else Nothing
     where
        max# x y = if isTrue# (x ># y) then x else y
        min# x y = if isTrue# (x <# y) then x else y

    ptrCmp :: Span a -> Span a -> Maybe Ordering
    ptrCmp (Span o _ xs) (Span p _ ys) = case samePtr xs ys of
        True | isTrue# (o <# p) -> Just LT
            | isTrue# (o ># p) -> Just GT
            | otherwise        -> Just EQ
        False -> Nothing

    slice :: Int -> Int -> Span a -> Span a
    slice (I# d) (I# n) (Span o l xs) = if pos# d && pos# n && isTrue# (d +# n <=# l)
        then Span (o +# d) n xs
        else error "slice indices out of bounds"
    
    trims :: Int -> Int -> Span a -> Span a
    trims (I# l) (I# r) (Span o n xs) = let n' = n -# r -# l in
        if pos# l && pos# r && pos# n'
            then Span (o +# l) n' xs
            else error "trims offset out of bounds"

instance Foldable Span where
    foldl :: forall a b. (b -> a -> b) -> b -> Span a -> b
    foldl f b0 (Span o l xs) = for o b0 where
        n = o +# l
        for :: Int# -> b -> b
        for i b = if isTrue# (i <# n)
            then for (i +# 1#)$ f b (xs `at#` i)
            else b
    foldr :: forall a b. (a -> b -> b) -> b -> Span a -> b
    foldr f b0 (Span o l xs) = forr h b0 where
        h = o +# l -# 1#
        forr :: Int# -> b -> b
        forr i b = if isTrue# (i <# o)
            then b
            else forr (i -# 1#)$ f (xs `at#` i) b
    null (Span _ l _) = isTrue# (l ==# 0#)
    length (Span _ l _) = I# l

instance Show a => Show (Span a) where
    showsPrec p xs = showsPrec p (toList xs)

instance Eq (Span a) where
    (Span o l xs) == (Span o' l' ys) = samePtr xs ys && isTrue# (o ==# o') && isTrue# (l ==# l')


-- * Construction
-- | Aliases an array as a span.
--   Discards index types completely, rebasing the array to 0.
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
            let siz = 128# in
            let !(# s1, mut #) = newSmallArray# siz (undefined :: a) s in
            let !(# s2, cop #) = copy s1 siz mut 0# xs in
            (# s2, fromSArray# cop #)
    -- | Copies a list into an array
    --   - $1: state thread
    --   - $2: Capacity of $3
    --   - $3: Current array
    --   - $4: Number of inserted entries
    --   - $5: List to copy
    --   Returns: ( state thread, finished array, array length )
    copy :: State# s -> Int# -> SmallMutableArray# s a -> Int# -> [a] -> (# State# s, SmallArray# a #)
    copy s _ arr l    []  =
            let !(# s1, arr1 #) = resizeSmallMutableArray# arr l undefined s in
            let !(# s2, arr2 #) = unsafeFreezeSmallArray# arr1 s1 in
            (# s2, arr2 #)
    copy s c arr l    xs  | isTrue# (c ==# l) =
            let l' = 2# *# l in
            let !(# s', arr' #) = resizeSmallMutableArray# arr l' undefined s in
            copy s' l' arr' l xs
    copy s c arr l (x:xs) =
            let s' = writeSmallArray# arr l x s in
            copy s' c arr (l +# 1#) xs


-- * Misc functions
-- | Indexes into a span
at :: Span a -> Int -> Maybe a
at (Span o l xs) (I# i) = if pos# i && isTrue# (i <# l)
    then Just (xs `at#` (i +# o))
    else Nothing
