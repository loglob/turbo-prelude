{-# LANGUAGE UndecidableInstances #-}

module Turbo.Internal.Classes where

import Control.Lens qualified as L
import Control.Monad.State (MonadState (..), execState)
import Turbo.RootPrelude

-- * uncons

-- | A read-only variant of `Cons`
class Uncons t a where
    uncons :: t -> Maybe (a, t)

-- | `Uncons` is a subclass of `Simple Cons`
instance {-# OVERLAPPABLE #-} (Cons t t a a) => Uncons t a where
    uncons :: t -> Maybe (a, t)
    uncons = L.uncons

infixr 5 :<
pattern (:<) :: (Uncons t a) => a -> t -> t
pattern (:<) x xs <- (uncons -> Just (x, xs))

-- * unsnoc

-- | A read-only variant of `Snoc`
class Unsnoc t a where
    unsnoc :: t -> Maybe (t, a)

-- | `Unsnoc` is a subclass of `Simple Snoc`
instance {-# OVERLAPPABLE #-} (Snoc t t a a) => Unsnoc t a where
    unsnoc :: t -> Maybe (t, a)
    unsnoc = L.unsnoc

infixl 5 :>
pattern (:>) :: (Unsnoc t a) => t -> a -> t
pattern (:>) x xs <- (unsnoc -> Just (x, xs))

-- * AtConst

-- | A weaker variant of `At` that cannot be written to
class AtConst m where
    infixl 9 @

    -- | Resolves a possibly absent index
    (@) :: m -> Index m -> Maybe (IxValue m)

instance {-# OVERLAPPABLE #-} (Ixed m) => AtConst m where
    x @ i = execState ((ix i) (\y -> put (Just y) >> return y) x) Nothing

-- * AtConstRev

-- | A reversed variant of `AtConst`
class AtConstRev xs x where
    -- | Variant of `@` that indexes from the right, with the last element at `0`
    (@~) :: xs -> Int -> Maybe x

-- | The default, O(n) instance. Overlap with faster variants.
instance {-# OVERLAPPABLE #-} (Unsnoc xs x) => AtConstRev xs x where
    (@~) xs i =
        if i < 0
            then Nothing
            else do
                (ys, y) <- unsnoc xs
                if i == 0
                    then Just y
                    else ys @~ pred i
