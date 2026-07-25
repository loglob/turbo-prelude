module Turbo.Cast.Classes where
import Turbo.Prelude (TYPE)
import Data.Kind (Type)

-- | Expresses that a lifted type is a boxed variant of an underlying primitive
class Boxed (unboxed :: TYPE kind) (boxed :: Type) | boxed -> unboxed, unboxed -> boxed where
    box :: unboxed -> boxed
    unbox :: boxed -> unboxed

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

-- | Expresses that two bit-fields are the same width, but one has a sign bit and the other doesn't
class IsSigned (signed :: TYPE a) (unsigned :: TYPE b) | signed -> unsigned, unsigned -> signed where
    -- | Converts a signed number to an equally wide unsigned type
    --   Undefined for values that exceed the target type's capacity
    sign :: unsigned -> signed
    -- | Like sign_ but saturates (becomes the maximum value) when the input exceeds the bounds
    signSat :: unsigned -> signed

    -- | Converts a signed number to an equally wide unsigned type
    --   Undefined for negative values
    unsign :: signed -> unsigned
    -- | Like sign_ but saturates (becomes 0) when the input is negative
    unsignSat :: signed -> unsigned
