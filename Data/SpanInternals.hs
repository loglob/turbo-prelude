-- | Implements internal operations reused over `Data.Span` and `Data.LargeText`
module Data.SpanInternals where
import GHC.Exts
import Turbo.RootPrelude

-- | `compare` on unlifted `Int#`
cmp# :: Int# -> Int# -> Ordering
cmp# x y | x `lt#` y = LT
         | x `gt#` y = GT
         | True      = EQ

eq# :: Int# -> Int# -> Bool
eq# x y = isTrue# (x ==# y)

lt# :: Int# -> Int# -> Bool
lt# x y = isTrue# (x <# y)

leq# :: Int# -> Int# -> Bool
leq# x y = isTrue# (x <=# y)

gt# :: Int# -> Int# -> Bool
gt# x y = isTrue# (x ># y)

geq# :: Int# -> Int# -> Bool
geq# x y = isTrue# (x >=# y)

inc# :: Int# -> Int#
inc# x = x +# 1#

-- | Generic helper for `extends`. Takes left extension, right extension, cur offset, cur length, total capacity
_extends :: Int# -> Int# -> Int# -> Int# -> Int# -> (# Int#, Int# #)
_extends n m o l z = if n `geq#` 0#  &&  m `geq#` 0#  &&  n `leq#` o  &&  (n +# m +# o +# l) `leq#` z
        then (# o -# n, l +# n +# m #)
        else (# -1#, -1# #)

-- | Generic helper for `isSliceOf` that takes (offset, length) pairs
_isSliceOf :: Int# -> Int# -> Int# -> Int# -> Maybe Int
_isSliceOf o l o' l' = if isTrue# (o >=# o') && isTrue# (o +# l <=# o' +# l')
        then Just (I# (o -# o'))
        else Nothing

-- | Generic helper or `overlap` on (offset, length) pairs.
--   Returns (-1,-1) to signal that there is no overlap
_overlap :: Int# -> Int# -> Int# -> Int# -> (# Int#, Int# #)
_overlap o l o' l' =
        let oR = max# o o' in
        let hR = min# (o +# l) (o' +# l') in
        if isTrue# (oR <=# hR)
            then (# oR, (hR -# oR) #)
            else (# -1#, -1# #)
     where
        max# x y = if isTrue# (x ># y) then x else y
        min# x y = if isTrue# (x <# y) then x else y

-- | Bounds-checks a slicing operation
-- takes (offset, length) pairs, slice first, then array
_slice :: Int# -> Int# -> Int# -> Int# ->Int#
_slice d n o l = if d `geq#` 0#  &&  n `geq#` 0#  &&  (d +# n) `leq#` l
        then o +# d
        else -1#
