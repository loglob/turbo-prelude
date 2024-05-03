-- | Type for `MapperT` to build `Span`s
module Control.SpanBuilder (SpanBuilder (), runBuilder) where

import Control.Lens
import Data.Internal.MutSpan
import Data.Internal.Span
import GHC.Exts
import GHC.ST
import Turbo.Prelude

-- | Internal ST-like constructor function
type ST' s (a :: TYPE (BoxedRep l)) = State# s -> (# State# s, MutSpan s a #)

{- | Constructs a span efficiently. Can be used with `MapperT`.

(!) Its `AsEmpty` and `Snoc` instances do not fulfill Prism laws, they are write only
-}
newtype SpanBuilder (x :: TYPE (BoxedRep l)) = SB (forall s. ST' s x)

-- | Retrieves a span from a builder
runBuilder :: SpanBuilder a -> Span a
runBuilder (SB f) = runST $ ST \s -> let !(# s1, xs #) = f s in unsafeFreezeSpan xs s1

instance AsEmpty (SpanBuilder a) where
    _Empty :: Prism' (SpanBuilder a) ()
    _Empty = prism' (\() -> SB newMutSpan) (const Nothing)

instance Snoc (SpanBuilder a) (SpanBuilder a) a a where
    _Snoc :: Prism' (SpanBuilder a) (SpanBuilder a, a)
    _Snoc = prism' f (const Nothing)
      where
        f (SB h, b) = SB \s -> let !(# s1, xs #) = h s in snocMutSpan xs b s1
