module Data.Span (
    ISpan (..),
    Span (),
    USpan (),
    fromArray,
    fromArray#,
    fromList,
    fromSArray#,
    sliceEnd,
    trims,
    fromListU,
    fromBytes#,
) where

import Control.Extra
import Control.Lens
import Data.Array.Byte
import Data.Primitive (Prim (indexByteArray#, sizeOfType#, writeByteArray#))
import Data.Proxy
import Data.SpanInternals
import Data.Text qualified as T (length, measureOff)
import Data.Text.Internal as T
import GHC.Arr as A
import GHC.Base
import GHC.Exts (resizeSmallMutableArray#)
import GHC.ST
import Turbo.RootPrelude hiding (for)

-- * Internals

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

-- * Type definition

-- ** Span class

{- | A collection type that permits comparing the underlying pointers,
  and creating 0-copy slices
-}
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
    --   Returns an empty span if the spans are exactly next to another.
    overlap :: s -> s -> Maybe s

    -- | Compares the underlying pointers of two spans
    --   Returns `Nothing` if the spans point into different arrays,
    --   compares the starting address of the spans otherwise.
    ptrCmp :: s -> s -> Maybe Ordering

    -- | Creates a sub-span from an offset and a length
    --   Partial if indices are out of bounds
    slice :: Int -> Int -> s -> s

{- | Like `slice`, but indexes from the end of the span rather than the start.
  Partial if indices are out of bounds.

  (!) The last element is at index 1
-}
sliceEnd :: (ISpan s) => Int -> Int -> s -> s
sliceEnd n m s = slice (size s - n) m s

-- | Trims the $1 leftmost and $2 rightmost elements of $3
trims :: (ISpan s) => Int -> Int -> s -> s
trims l r s =
    let z = size s
     in if l < 0 || r < 0 || l + r > z
            then error "trims indices out of range"
            else slice l (size s - l - r) s

-- ** Array Span

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

{- | tail-recursive for loop with foldl-operator
  Bounds given by low (inclusive) and high (exclusive) value
-}
for :: forall a b. (b -> a -> b) -> (Int# -> a) -> Int# -> Int# -> b -> b
for op at i0 hi = loop i0
  where
    loop :: Int# -> b -> b
    loop i b =
        if i `lt#` hi
            then loop (inc# i) (b `op` at i)
            else b

{- | tail-recursive reverse for loop with foldr-operator
  Bounds given by low (inclusive) and high (exclusive) value
-}
forr :: forall a b. (a -> b -> b) -> (Int# -> a) -> Int# -> Int# -> b -> b
forr op at lo hi = loop (hi -# 1#)
  where
    loop :: Int# -> b -> b
    loop i b =
        if i `geq#` lo
            then loop (i -# 1#) (at i `op` b)
            else b

instance Foldable Span where
    foldl :: forall a b. (b -> a -> b) -> b -> Span a -> b
    foldl f b0 (Span o l xs) = for f (at# xs) o (o +# l) b0
    foldr :: forall a b. (a -> b -> b) -> b -> Span a -> b
    foldr f b0 (Span o l xs) = forr f (at# xs) o (o +# l) b0
    null (Span _ l _) = l `eq#` 0#
    length (Span _ l _) = I# l

instance (Show a) => Show (Span a) where
    showsPrec p xs = showsPrec p (toList xs)

-- ** Unpacked Array Spans

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
    extends :: Int -> Int -> USpan a -> USpan a
    extends (I# n) (I# m) (USpan o l arr) = case _extends n m o l (capacity (Proxy :: Proxy a) arr) of
        (# -1#, _ #) -> error "extends indices out of range"
        (# o', l' #) -> USpan o' l' arr
    isSliceOf :: USpan a -> USpan a -> Maybe Int
    isSliceOf (USpan o n xs) (USpan p m ys) = if isTrue# (unsafePtrEquality# xs ys) then _isSliceOf o n p m else Nothing
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

-- ** Text Span

-- | Variant of `measureOff` that checks the text is long enough
measureOff' :: Int -> Text -> Maybe Int
measureOff' 0 _ = Just 0
measureOff' n _ | n < 0 = error "measureOff' with negative index"
measureOff' n t = case T.measureOff n t of
    c | c > 0 -> Just c
    _ -> Nothing

-- | Instance for Text indexed in chars
instance ISpan Text where
    baseSpan :: Text -> Text
    baseSpan (Text (ByteArray xs) _ _) = Text (ByteArray xs) 0 (I# (sizeofByteArray# xs))
    extends :: Int -> Int -> Text -> Text
    extends n m (Text xs o l) = ext
      where
        ext =
            if n > o || o + l + m > total
                -- Check upfront whether 1 char/byte could fulfill the extend request
                then error "extends indices out of range"
                else case (lOff, rOff) of
                    (Just x, Just y) -> Text xs (o - x) (l + x + y)
                    _ -> error ""
        total = let !(ByteArray arr) = xs in I# (sizeofByteArray# arr)
        rRest = Text xs (o + l) (total - o - l)
        lRest = Text xs 0 o
        rOff = measureOff' m rRest
        lOff =
            if n == 0
                then Just 0
                else
                    let z = size lRest
                     in case compare n z of
                            LT -> fmap (o -) (measureOff' (z - n) lRest) -- < this shouldn't be able to fail
                            EQ -> Just o
                            GT -> Nothing

    isSliceOf :: Text -> Text -> Maybe Int
    isSliceOf (Text (ByteArray xs) o n) (Text (ByteArray ys) p m) = case unsafePtrEquality# xs ys of
        1# | o >= p && o + n <= p + m -> Just (T.length (Text (ByteArray xs) p (o - p)))
        _ -> Nothing
    size :: Text -> Int
    size = T.length
    overlap :: Text -> Text -> Maybe Text
    overlap (Text (ByteArray xs) o n) (Text (ByteArray ys) p m) = case unsafePtrEquality# xs ys of
        1# ->
            let oR = max o p
             in let hR = min (o + n) (p + m)
                 in if oR <= hR
                        then Just (Text (ByteArray xs) oR (hR - oR))
                        else Nothing
        _ -> Nothing
    ptrCmp :: Text -> Text -> Maybe Ordering
    ptrCmp (Text (ByteArray xs) o _) (Text (ByteArray ys) p _) = case unsafePtrEquality# xs ys of
        1# -> Just (compare o p)
        _ -> Nothing
    slice :: Int -> Int -> Text -> Text
    slice o n t@(Text arr p l) = case measureOff' o t of
        Nothing -> error "slice index out of range"
        Just d ->
            let t' = Text arr (p + d) (l - d)
             in case measureOff' n t' of
                    Nothing -> error "slice index out of range"
                    Just z -> Text arr (p + d) z

-- * Construction

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

fromBytes# :: forall a. (Prim a) => ByteArray# -> USpan a
fromBytes# bs = USpan 0# (capacity (Proxy :: Proxy a) bs) bs

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
