-- | Type for `MapperT` to build `Span`s
module Control.SpanBuilder (SpanBuilder (), runBuilder) where

import Control.Lens
import Data.Internal.ISpan
import Data.Internal.Span
import GHC.Err (undefined)
import GHC.Exts
import GHC.ST
import Turbo.Prelude

-- | Span-like type with mutable data
data SpanBuilderState s (a :: TYPE (BoxedRep l)) = S Int# Int# (SmallMutableArray# s a)

-- | Internal ST-like constructor function
type ST' s (a :: TYPE (BoxedRep l)) = State# s -> (# State# s, SpanBuilderState s a #)

{- | Constructs a span efficiently. Can be used with `MapperT`.

(!) Its `AsEmpty` and `Snoc` instances do not fulfill Prism laws, they are write only
-}
newtype SpanBuilder (x :: TYPE (BoxedRep l)) = SB (forall s. ST' s x)

-- | Retrieves a span from a builder
runBuilder :: SpanBuilder a -> Span a
runBuilder (SB f) = runST $ ST \s ->
    let
        !(# s1, S _ n ys #) = f s
        s2 = shrinkSmallMutableArray# ys n s1
        !(# s3, zs #) = unsafeFreezeSmallArray# ys s2
     in
        (# s3, fromSArray# zs #)

instance AsEmpty (SpanBuilder a) where
    _Empty :: Prism' (SpanBuilder a) ()
    _Empty = prism' (\() -> SB f) (const Nothing)
      where
        f s =
            let
                c0 = 64#
                !(# s1, xs #) = newSmallArray# c0 undefined s
                st = S c0 0# xs
             in
                (# s1, st #)

instance Snoc (SpanBuilder a) (SpanBuilder a) a a where
    _Snoc :: Prism' (SpanBuilder a) (SpanBuilder a, a)
    _Snoc = prism' f (const Nothing)
      where
        f (SB h, b) = SB \s ->
            let
                !(# s1, xs #) = h s
                !(# s2, S c n ys #) = ensure# 1# xs s1
                s3 = writeSmallArray# ys n b s2
             in
                (# s3, S c (inc# n) ys #)
        ensure# :: Int# -> SpanBuilderState s b -> ST' s b
        ensure# k x@(S c n buf) s =
            let c' = newCap c
             in if c' `gt#` c
                    then
                        let !(# s', buf' #) = resizeSmallMutableArray# buf c' undefined s in (# s', S c' n buf' #)
                    else
                        (# s, x #)
          where
            newCap a = if (n +# k) `gt#` a then newCap (2# *# a) else a
