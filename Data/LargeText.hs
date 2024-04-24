-- | Implements large (eager) Text with efficient indexing
module Data.LargeText (LargeText (), Position (..), charAtPos, fromText, getLine, indexPos, posOfChar, toText, uncons, unconsPos) where

import Control.Extra
import Control.Lens (Index, IxValue)
import Data.Internal.ISpan
import Data.Primitive (ByteArray (..))
import Data.Text qualified as T
import Data.Text.Internal (Text (..))
import GHC.Base
import GHC.Exts
import GHC.ST
import Turbo.Prelude hiding (getLine, uncons)

-- * Type definitions

-- | Placed every 512 bytes to track char <-> byte index mapping
data Marker = Marker
    { -- | The byte index of the first byte of the next character
      byteOffset :: Int#,
      -- | The line number of the next character
      -- Increments after newlines.
      line :: Int#,
      -- | The row number of the next character
      col :: Int#
    }

instance Show Marker where
    showsPrec 0 (Marker x y z) = ("Marker " ++) . shows (I# x) . (' ' :) . shows (I# y) . (' ' :) . shows (I# z)
    showsPrec _ m = ('(' :) . showsPrec 0 m . (')' :)

{- | A line:column position in an input document.
 Always relative to the `baseSpan` of any sub-span
-}
data Position = Position
    { -- | The 1-based line number of this input position.
      --  Incremented after newlines, so that the terminating newline
      --  is still part of the line it ends.
      lineNum :: Int,
      -- | The 1-based column number of this input position.
      columnNum :: Int
    }
    deriving (Eq, Ord)

instance Show Position where
    showsPrec _ (Position l c) = shows l . (':' :) . shows c

{- | A type for large text.
 Uses an offset table internally for tracking character index, line and column position,
 so that all indexing and slicing operations are O(1).

 It should be used when a large (>> 512 bytes) `Text` is repeatedly searched for large character indices,
   or mapping from/to file positions is required.
 It should be avoided if a large `Text` is whittled down via slicing and working with the larger `Text` is never required,
   as it may pin a large amount of memory, preventing GC.
-}
data LargeText = LargeText
    { -- | The underlying contiguous byte array
      bytes :: ByteArray#,
      -- | Markers places exactly every 512 characters
      --   The zero-marker is omitted, so index 0 corresponds to character index 512.
      markers :: SmallArray# Marker,
      charOffset :: Int#,
      charCount :: Int#
    }

-- * Internal functions
zeroMarker :: Marker
zeroMarker = Marker 0# 1# 1#

-- | Gets the chunk starting at the given byte offset. Ignores `charCount` and `charOffset`.
chunkFromByte :: LargeText -> Int# -> Text
chunkFromByte txt o = let bs = bytes txt in Text (ByteArray bs) (I# o) (I# (sizeofByteArray# bs -# o))

marker' :: SmallArray# Marker -> Int# -> Marker
marker' ms m = case m of
    0# -> zeroMarker
    _ -> let (# x #) = indexSmallArray# ms (m -# 1#) in x

-- | indexes the marker array, or returns a zeroMarker. Index must be <= marker count.
marker :: LargeText -> Int# -> Marker
marker txt i = marker' (markers txt) i

{- | Gets the chunk containing a character index. Returns:
 * The index of the character inside the chunk
 * The text segment beginning with that chunk
 * The marker starting the chunk
-}
chunkOfChar :: LargeText -> Int# -> (# Int#, Text, Marker #)
chunkOfChar txt c =
    let gc = charOffset txt +# c
        !(# m, o #) = gc `divModInt#` 512#
        mk = marker txt m
     in (# o, chunkFromByte txt (byteOffset mk), mk #)

-- | Checks if a character index is in bounds
inBounds :: LargeText -> Int# -> Bool
inBounds txt c = c `geq#` 0# && c `lt#` charCount txt

-- | Computes the position after processing some characters
computePos :: Int# -> Int# -> [Char] -> (# Int#, Int# #)
computePos l r [] = (# l, r #)
computePos l _ ('\n' : cs) = computePos (inc# l) 1# cs
computePos l r (_ : cs) = computePos l (inc# r) cs

finalChunk :: LargeText -> Text
finalChunk txt =
    let ms = markers txt
        m = marker' ms (sizeofSmallArray# ms)
     in chunkFromByte txt (byteOffset m)

-- | Checks if two large texts use the same pointers
samePtrs :: LargeText -> LargeText -> Bool
samePtrs a b = isTrue# (unsafePtrEquality# (bytes a) (bytes b)) && isTrue# (unsafePtrEquality# (markers a) (markers b))

app :: (Int# -> Int# -> (a :: TYPE rep)) -> LargeText -> a
app f txt = f (charOffset txt) (charCount txt)

-- * Exposed Interfaces

-- ** AtConst indexing
type instance Index LargeText = Int
type instance IxValue LargeText = Char

instance AtConst LargeText where
    (@) :: LargeText -> Int -> Maybe Char
    txt @ (I# i) =
        if inBounds txt i
            then let !(# d, ch, _ #) = chunkOfChar txt i in Just (T.index ch (I# d))
            else Nothing

-- ** Regular collection interfaces

-- | Retrieves the character at some index and its position. O(1)
indexPos :: LargeText -> Int -> Maybe (Char, Position)
indexPos txt (I# i) =
    if inBounds txt i
        then
            let !(# d, ch, m #) = chunkOfChar txt i
                (l, r) = T.splitAt (I# d) ch
                !(# ln, cn #) = computePos (line m) (col m) (T.unpack l)
             in Just (T.head r, Position (I# ln) (I# cn))
        else
            Nothing

-- | Attempts to split off the leftmost character. O(1)
uncons :: LargeText -> Maybe (Char, LargeText)
uncons t@(LargeText bs ms o l) = (t @ 0) <& LargeText bs ms (inc# o) (l -# 1#)

-- | Attempts to split off the leftmost character and returns its position. O(1)
unconsPos :: LargeText -> Maybe (Char, Position, LargeText)
unconsPos t@(LargeText bs ms o l) = indexPos t 1 <.& LargeText bs ms (inc# o) (l -# 1#)

-- ** Positions interface

{- | Determines the 1-based file position of a character by its index.
 O(1)
-}
posOfChar :: LargeText -> Int -> Maybe Position
posOfChar txt (I# i) =
    if inBounds txt i
        then
            let !(# d, ch, m #) = chunkOfChar txt i
                !(# r, l #) = computePos (line m) (col m) (take (I# d) (unpack ch))
             in Just (Position (I# r) (I# l))
        else
            Nothing

-- | Searches for the character index of a position, if it exists. O(log n)
charAtPos :: LargeText -> Position -> Maybe Int
charAtPos tx p = case seekMarker tx p of
    (# -1#, _ #) -> Nothing
    (# i, m #) -> case seekPos tx p m of
        -1# -> Nothing
        -- possible optimization: perform bounds checking earlier on the results of seekMarker and seekPos
        j ->
            let k = i *# 512# +# j -# charOffset tx
             in if inBounds tx k then Just (I# k) else Nothing
  where
    -- \| Searches a position within a chunk, if it exists. Returns character offset within chunk.
    seekPos :: LargeText -> Position -> Marker -> Int#
    seekPos txt (Position (I# lW) (I# cW)) (Marker b l0 c0) = f l0 c0 0# (chunkFromByte txt b)
      where
        pCmp :: Int# -> Int# -> Ordering
        pCmp l c = case cmp# l lW of
            EQ -> cmp# c cW
            x -> x
        f :: Int# -> Int# -> Int# -> Text -> Int#
        f l c n txt = case pCmp l c of
            GT -> -1#
            EQ -> n
            LT -> case T.uncons txt of
                Nothing -> -1#
                Just ('\n', t') -> f (inc# l) 1# (inc# n) t'
                Just (_, t') -> f l (inc# c) (inc# n) t'

    -- \| Seeks the last marker before a position. Indexes like `marker`.
    seekMarker :: LargeText -> Position -> (# Int#, Marker #)
    seekMarker txt (Position (I# ln) (I# co)) =
        let !(# lo, hi #) = markerRange txt
         in case binSeekSArr# (markers txt) before lo hi of
                (# -1#, _ #) ->
                    if before zeroMarker
                        then (# 0#, zeroMarker #)
                        else (# -1#, undefined #)
                (# i, x #) -> (# inc# i, x #)
      where
        -- \| Returns the range of markers addressed by a large text span
        markerRange :: LargeText -> (# Int#, Int# #)
        markerRange (LargeText _ _ o l) = (# o `divInt#` 512#, inc# ((o +# l) `divInt#` 512#) #)
        -- \| Binary-searches a monotonous predicate.
        --   Returns `-1#` if the entire array is false.
        --   Accepts (included, excluded) bounds, assumed to be >= 0.
        binSeek# :: (Int# -> Bool) -> Int# -> Int# -> Int#
        binSeek# f = seek
          where
            seek :: Int# -> Int# -> Int#
            seek l h =
                if l `geq#` h
                    then
                        if f l
                            then l
                            else -1#
                    else
                        let m = l +# ((h -# l +# 1#) `divInt#` 2#)
                         in if f m
                                then seek m h
                                else seek l (m -# 1#)
        binSeekSArr# :: SmallArray# a -> (a -> Bool) -> Int# -> Int# -> (# Int#, a #)
        binSeekSArr# arr f l h = case binSeek# (\i -> let (# x #) = indexSmallArray# arr i in f x) l h of
            -1# -> (# -1#, undefined #)
            j -> let (# y #) = indexSmallArray# arr j in (# j, y #)
        before :: Marker -> Bool
        before (Marker _ l c) = case cmp# l ln of
            LT -> True
            EQ -> c `leq#` co
            GT -> False

{- | Gets a line by its 1-based index, if such a line exists.
 Contains the terminating newline, if there is one.
 O(log n)
-}
getLine :: LargeText -> Int -> Maybe LargeText
getLine txt ln =
    charAtPos txt (Position ln 1) <§ \i ->
        case charAtPos txt (Position (ln + 1) 1) of
            -- possible improvement: reuse offsets computed during the first charAtPos
            Nothing -> slice i (size txt - i) txt
            Just j -> slice i (j - i) txt

instance ISpan LargeText where
    baseSpan :: LargeText -> LargeText
    baseSpan txt@(LargeText bs ms _ _) =
        let mC = sizeofSmallArray# ms
            !(I# z) = T.length $ finalChunk txt
         in LargeText bs ms 0# ((512# *# mC) +# z)

    bounds :: LargeText -> LargeText -> Maybe LargeText
    bounds a b =
        if samePtrs a b
            then let !(# o, l #) = _bounds `app` a `app` b in Just $ a{charOffset = o, charCount = l}
            else Nothing

    extends :: Int -> Int -> LargeText -> LargeText
    extends (I# n) (I# m) txt@(LargeText bs ms o l) =
        if n `leq#` o && totalCharsGE txt (o +# l +# m)
            then LargeText bs ms (o -# n) (l +# n +# m)
            else error "extends indices out of range"
      where
        -- \| checks if the total number of chars in $1 is geq $2
        totalCharsGE :: LargeText -> Int# -> Bool
        totalCharsGE txt k =
            let !(# x, y #) = k `divModInt#` 512#
             in case cmp# x (sizeofSmallArray# (markers txt)) of
                    LT -> True
                    GT -> False
                    EQ -> y `eq#` 0# || T.measureOff (I# y) (finalChunk txt) > 0

    isSliceOf :: LargeText -> LargeText -> Maybe Int
    isSliceOf a b = if samePtrs a b then _isSliceOf `app` a `app` b else Nothing

    size :: LargeText -> Int
    size txt = I# (charCount txt)

    overlap :: LargeText -> LargeText -> Maybe LargeText
    overlap a b =
        if samePtrs a b
            then case _overlap `app` a `app` b of
                (# -1#, _ #) -> Nothing
                (# o, l #) -> Just (a{charOffset = o, charCount = l})
            else Nothing

    ptrCmp :: LargeText -> LargeText -> Maybe Ordering
    ptrCmp a b =
        if samePtrs a b
            then Just (cmp# (charOffset a) (charOffset b))
            else Nothing

    slice :: Int -> Int -> LargeText -> LargeText
    slice (I# o) (I# n) txt = case _slice o n `app` txt of
        -1# -> error "Slice indices out of bound"
        p -> txt{charOffset = p, charCount = n}

-- | Converts a text into a large text. O(n), doesn't copy (but allocates new memory)
fromText :: Text -> LargeText
fromText = \t -> runST (ST (f t))
  where
    f :: Text -> State# s -> (# State# s, LargeText #)
    f txt@(Text (ByteArray bs) _ _) s0 =
        let !(# s1, arr #) = newSmallArray# 8# (undefined :: Marker) s0
            !(# s2, cc, ms #) = initMarkers 8# arr 0# txt s1
         in (# s2, LargeText bs ms 0# cc #)
    advanceMarker :: Marker -> Text -> Either Int (Marker, Text)
    advanceMarker m0 t0 = adv 0# m0 t0
      where
        adv :: Int# -> Marker -> Text -> Either Int (Marker, Text)
        adv 512# m t = Right (m, t)
        adv n m t =
            -- Text may re-arrange memory internally to avoid GC pinning
            let !(Text _ _ (I# l), t') = T.splitAt 1 t
             in case t @ 0 of
                    Nothing -> Left (I# n)
                    Just '\n' -> adv (inc# n) (m{line = 1# +# line m, col = 1#, byteOffset = l +# byteOffset m}) t'
                    Just _ -> adv (inc# n) (m{col = 1# +# col m, byteOffset = l +# byteOffset m}) t'
    -- \| creates the markers array.
    -- \$1: current capacity of $2
    -- \$2: current array
    -- \$3: current number of markers
    -- \$4: current text suffix
    -- \$5: state thread
    initMarkers :: Int# -> SmallMutableArray# s Marker -> Int# -> Text -> State# s -> (# State# s, Int#, SmallArray# Marker #)
    initMarkers c arr n txt s
        | n `geq#` c =
            let c' = c *# 2#
                !(# s', arr' #) = resizeSmallMutableArray# arr c' undefined s
             in initMarkers c' arr' n txt s'
        | otherwise =
            let !(# s1, m #) = if n `eq#` 0# then (# s, zeroMarker #) else readSmallArray# arr (n -# 1#) s
             in case advanceMarker m txt of
                    Left (I# o) ->
                        let s2 = shrinkSmallMutableArray# arr n s1
                            !(# s3, arr' #) = unsafeFreezeSmallArray# arr s2
                         in (# s3, o +# 512# *# n, arr' #)
                    Right (!m', !t') ->
                        let s2 = writeSmallArray# arr n m' s1
                         in initMarkers c arr (inc# n) t' s2

-- | Converts large text into normal text. O(1), doesn't copy.
toText :: LargeText -> Text
toText txt@(LargeText bs _ _ l) = let bO = toByte 0# in Text (ByteArray bs) bO (toByte l - bO)
  where
    -- \| Translates a character offset into a byte offset
    toByte :: Int# -> Int
    toByte i = let !(# o, x, m #) = chunkOfChar txt i in (I# (byteOffset m)) + T.measureOff (I# o) x
