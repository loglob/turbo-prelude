{-# OPTIONS_GHC -Wno-orphans #-}
module Turbo.Cast(module E) where

import GHC.Exts
import Data.Bits
import Turbo.RootPrelude
import Turbo.Cast.Classes as E
import Turbo.Cast.TH
import Data.Primitive
import GHC.StableName (StableName)
import GHC.Conc (TVar, ThreadId)
import GHC.Weak (Weak)
import GHC.Stack.CloneStack (StackSnapshot)
import GHC.Stable (StablePtr)

$(deriveBoxed ''SmallArray)
$(deriveBoxed ''SmallMutableArray)
$(deriveBoxed ''Array)
$(deriveBoxed ''MutableArray)
$(deriveBoxed ''ByteArray)
$(deriveBoxed ''MutableByteArray)

$(deriveBoxed ''MutVar)
$(deriveBoxed ''StableName)
$(deriveBoxed ''StablePtr)
$(deriveBoxed ''StackSnapshot)
$(deriveBoxed ''ThreadId)
$(deriveBoxed ''TVar)
$(deriveBoxed ''Weak)

$(deriveBoxed ''Char)
$(deriveBoxed ''Float)
$(deriveBoxed ''Double)

$makeBoxedIntegers
$makeExtend
$makeIsSigned

instance Boxed Addr# (Ptr ()) where
    box = Ptr
    unbox (Ptr x) = x
