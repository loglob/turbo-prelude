module Turbo.Cast.Classes where
import Turbo.Prelude (TYPE)
import Data.Kind (Type)
import Turbo.RootPrelude

-- | Expresses that a lifted type is a boxed variant of an underlying primitive
class Boxed (unboxed :: TYPE kind) (boxed :: Type) | boxed -> unboxed, unboxed -> boxed where
    box :: unboxed -> boxed
    unbox :: boxed -> unboxed

pattern Boxed :: Boxed u b => u -> b
pattern Boxed y <- !(unbox -> y) where
    Boxed x = box x

-- | The property that `long` is a bitfield with more bits than `short`.
--   Implements safe widening and narrowing
class (long :: TYPE a) :>: (short :: TYPE b) where
    -- | Extends a bit field to become a wider type.
    --   Performs sign extension only when widening from a signed type to another signed type.
    --   Widening a signed type to an unsigned type is not permitted.
    extend :: short -> long
    -- | Truncates a bit-field into a smaller type, discarding the most significant bits.
    --   Total but may produce negative numbers for signed result types.
    narrow :: long -> short

data Signum = NEGATIVE | ZERO | POSITIVE deriving (Show, Eq, Bounded, Enum, Ord)

instance Num Signum where
    ZERO + y    = y
    x    + ZERO = x
    x    + y    = if x == y then x else ZERO 
    x - y = x + (negate y)
    ZERO * _    = ZERO
    _    * ZERO = ZERO
    x    * y    = if x == y then POSITIVE else NEGATIVE
    negate ZERO = ZERO
    negate NEGATIVE = POSITIVE
    negate POSITIVE = NEGATIVE
    abs ZERO = ZERO
    abs _    = POSITIVE
    signum = id
    fromInteger 0 = ZERO
    fromInteger x = if x < 0 then NEGATIVE else POSITIVE


-- | Expresses that two bit-fields are the same width, but one has a sign bit and the other doesn't
class IsSigned (signed :: TYPE a) (unsigned :: TYPE b) | signed -> unsigned, unsigned -> signed where
    -- | Converts a signed number to an equally wide unsigned type
    --   Undefined for values that exceed the target type's capacity
    sign :: unsigned -> signed
    -- | Like sign but saturates (becomes the maximum value) when the input exceeds the bounds
    signSat :: unsigned -> signed

    -- | Converts a signed number to an equally wide unsigned type
    --   Undefined for negative values
    unsign :: signed -> unsigned 
    -- | Like unsign but saturates (becomes 0) when the input is negative
    unsignSat :: signed -> unsigned
    -- | The absolute value of a signed number
    abs# :: signed -> unsigned
