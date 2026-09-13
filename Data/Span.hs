module Data.Span ( ISpan(..), ArraySpan(), MutSpan(), USpan(), MutUSpan() ) where
import Turbo.RootPrelude
import Data.Span.Internal
import qualified Data.Vector as V
import Data.Primitive.Array (Array(..))
import Turbo.Internal.Classes
import Data.Array.Byte (ByteArray (..))
import qualified Data.Text as T
import Data.Text.Internal(Text(..))
import GHC.Err (error)
import Data.Foldable (length)

-- * Span instance for Vector
instance ISpan (V.Vector v) where
    baseSpanOff :: V.Vector v -> (V.Vector v, Int)
    baseSpanOff v =
        let (a, o, _) = V.toArraySlice v
         in (V.unsafeFromArraySlice a 0 (length a), o)

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


-- * Span instance for Text
-- | Variant of `measureOff` that checks the text is long enough
measureOff' :: Int -> Text -> Maybe Int
measureOff' 0 _ = Just 0
measureOff' n _ | n < 0 = error "measureOff' with negative index"
measureOff' n t = case T.measureOff n t of
    c | c > 0 -> Just c
    _ -> Nothing

-- | Instance for Text indexed in chars
instance ISpan Text where
    baseSpanOff :: Text -> (Text, Int)
    baseSpanOff (Text (ByteArray xs) byteOff _) =
        let
            bArr = ByteArray xs
            base = Text bArr 0 (I# (sizeofByteArray# xs))
            charOff = T.length (Text bArr 0 byteOff)
         in
            (base, charOff)

    bounds :: Text -> Text -> Maybe Text
    bounds (Text (ByteArray xs) (I# o) (I# n)) (Text (ByteArray ys) (I# p) (I# m)) = case unsafePtrEquality# xs ys of
        1# -> let !(# q, k #) = _bounds o n p m in Just $ Text (ByteArray xs) (I# q) (I# k)
        _ -> Nothing

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
                hR = min (o + n) (p + m)
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
