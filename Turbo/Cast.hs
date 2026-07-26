{-# OPTIONS_GHC -Wno-orphans #-}
module Turbo.Cast(module E) where

import GHC.Exts
import Data.Bits
import Turbo.RootPrelude
import Turbo.Cast.Classes as E
import Turbo.Cast.TH

$makeBoxedIntegers
$makeExtend
$makeIsSigned
