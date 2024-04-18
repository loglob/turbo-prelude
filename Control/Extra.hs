{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
module Control.Extra where
import Control.Lens
import Control.Monad.State
import Turbo.RootPrelude

-- | A weaker variant of `At` that cannot be written to 
class AtConst m where
    infixl 9 @
    -- | Resolves a possibly absent index
    (@) :: m -> Index m -> Maybe (IxValue m)

instance {-# OVERLAPPABLE #-} Ixed m => AtConst m where
    x @ i = execState ((ix i) (\y -> put (Just y) >> return y) x) Nothing

infixl 4 <@, @>, <@>

-- | Left functor wrapper of `@`
(<@) :: (AtConst m, Functor f) => f m -> Index m -> f (Maybe (IxValue m))
(<@) x i = fmap (@ i) x

-- | Right functor wrapper of `@`
(@>) :: (AtConst m, Functor f) => m -> f (Index m) -> f (Maybe (IxValue m))
(@>) x i = fmap (x @) i

-- | Applicative wrapper of `@`
(<@>) :: (AtConst m, Applicative f) => f m -> f (Index m) -> f (Maybe (IxValue m))
(<@>) = liftA2 (@)
