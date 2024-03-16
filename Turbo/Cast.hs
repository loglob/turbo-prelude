{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ExtendedLiterals #-}
{-# LANGUAGE BangPatterns #-}
module Turbo.Cast where
import Data.Bits
import Turbo.RootPrelude
import GHC.Exts
import GHC.Err (error)

-- * Sign Elimination
-- | Bit-casts int to word
--
-- (!) Partial if the argument is negative
unsign :: Int -> Word
unsign (I# i) = if isTrue# (i <# 0#) then error "Input to unsign is negative" else W# (int2Word# i)

-- | Bit-casts 8-bit signed to unsigned
--
-- (!) Partial if the argument is negative
unsign8 :: Int8 -> Word8
unsign8 (I8# i) = if isTrue# (i `ltInt8#` 0#Int8) then error "Input to unsign8 is negative" else W8# (int8ToWord8# i)

-- | Bit-casts 16-bit signed to unsigned
--
-- (!) Partial if the argument is negative
unsign16 :: Int16 -> Word16
unsign16 (I16# i) = if isTrue# (i `ltInt16#` 0#Int16) then error "Input to unsign16 is negative" else W16# (int16ToWord16# i)

-- | Bit-casts 32-bit signed to unsigned
--
-- (!) Partial if the argument is negative
unsign32 :: Int32 -> Word32
unsign32 (I32# i) = if isTrue# (i `ltInt32#` 0#Int32) then error "Input to unsign32 is negative" else W32# (int32ToWord32# i)

-- | Bit-casts 64-bit signed to unsigned
--
-- (!) Partial if the argument is negative
unsign64 :: Int64 -> Word64
unsign64 (I64# i) = if isTrue# (i `ltInt64#` 0#Int64) then error "Input to unsign64 is negative" else W64# (int64ToWord64# i)


-- * Sign Introduction
-- | Bit-casts a word to an int.
--
-- (!) Partial if the value does not fit into the bounds of Int
sign :: Word -> Int
sign (W# w) = let !(I# b) = finiteBitSize (0 :: Int)
    in if isTrue# (word2Int# (w `uncheckedShiftRL#` (b -# 1#)))
        then error "Input to sign exceeds the bounds of Int"
        else I# (word2Int# w)

-- | Bit-casts an 8-bit unsigned to signed
--
-- (!) Partial if the value does not fit into 7 bits
sign8 :: Word8 -> Int8
sign8 (W8# w) = if isTrue# (w `gtWord8#` 0x7F#Word8) then error "Input to sign8 exceeds the bounds of Int8" else I8# (word8ToInt8# w)

-- | Bit-casts a 16-bit unsigned to signed
--
-- (!) Partial if the value does not fit into 15 bits
sign16 :: Word16 -> Int16
sign16 (W16# w) = if isTrue# (w `gtWord16#` 0x7FFF#Word16) then error "Input to sign16 exceeds the bounds of Int16" else I16# (word16ToInt16# w)

-- | Bit-casts a 32-bit unsigned to signed
--
-- (!) Partial if the value does not fit into 31 bits
sign32 :: Word32 -> Int32
sign32 (W32# w) = if isTrue# (w `gtWord32#` 0x7FFF#Word32) then error "Input to sign32 exceeds the bounds of Int32" else I32# (word32ToInt32# w)

-- | Bit-casts a 64-bit unsigned to signed
--
-- (!) Partial if the value does not fit into 31 bits
sign64 :: Word64 -> Int64
sign64 (W64# w) = if isTrue# (w `gtWord64#` 0x7FFF#Word64) then error "Input to sign64 exceeds the bounds of Int64" else I64# (word64ToInt64# w)


-- * Word Extending
-- | Extends an 8-bit unsigned to 64 bits. Total.
extendW8 :: Word8 -> Word64
extendW8 (W8# w) = W64# (wordToWord64# (word8ToWord# w))

-- | Extends an 8-bit unsigned to 64 bits signed. Total.
--  Does not perform sign extension.
extendW8i :: Word8 -> Int64
extendW8i (W8# w) = I64# (intToInt64# (word2Int# (word8ToWord# w)))

-- | Extends an 8-bit unsigned to Word. Total.
extendW8' :: Word8 -> Word
extendW8' (W8# w) = W# (word8ToWord# w)

-- | Extends an 8-bit unsigned to Int. Total.
extendW8i' :: Word8 -> Int
extendW8i' (W8# w) = I# (int8ToInt# (word8ToInt8# w))

-- | Extends an 16-bit unsigned to 64 bits. Total.
extendW16 :: Word16 -> Word64
extendW16 (W16# w) = W64# (wordToWord64# (word16ToWord# w))

-- | Extends an 16-bit unsigned to 64 bits signed. Total.
--  Does not perform sign extension.
extendW16i :: Word16 -> Int64
extendW16i (W16# w) = I64# (intToInt64# (word2Int# (word16ToWord# w)))

-- | Extends a 16-bit unsigned to Word. Total.
extendW16' :: Word16 -> Word
extendW16' (W16# w) = W# (word16ToWord# w)

-- | Extends a 16-bit unsigned to Int. Total.
extendW16i' :: Word16 -> Int
extendW16i' (W16# w) = I# (int16ToInt# (word16ToInt16# w))

-- | Extends an 32-bit unsigned to 64 bits. Total.
extendW32 :: Word32 -> Word64
extendW32 (W32# w) = W64# (wordToWord64# (word32ToWord# w))

-- | Extends an 32-bit unsigned to 64 bits signed. Total.
--  Does not perform sign extension.
extendW32i :: Word32 -> Int64
extendW32i (W32# w) = I64# (intToInt64# (word2Int# (word32ToWord# w)))

-- | Extends a Word to 64 bits. Total.
extendW :: Word -> Word64
extendW (W# w) = W64# (wordToWord64# w)


-- * Int Extending
-- | Extends an 8-bit signed to 64 bit. Total.
extendI8 :: Int8 -> Int64
extendI8 (I8# i) = I64# (intToInt64# (int8ToInt# i))

-- | Extends an 8-bit signed to Int. Total.
extendI8' :: Int8 -> Int
extendI8' (I8# i) = I# (int8ToInt# i)

-- | Extends an 16-bit signed to 64 bit. Total.
extendI16 :: Int16 -> Int64
extendI16 (I16# i) = I64# (intToInt64# (int16ToInt# i))

-- | Extends an 16-bit signed to Int. Total.
extendI16' :: Int16 -> Int
extendI16' (I16# i) = I# (int16ToInt# i)

-- | Extends an 32-bit signed to 64 bit. Total.
extendI32 :: Int32 -> Int64
extendI32 (I32# i) = I64# (intToInt64# (int32ToInt# i))
