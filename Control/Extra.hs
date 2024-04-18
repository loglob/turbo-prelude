{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Extra where
import Control.Lens
import Turbo.RootPrelude
import Turbo.Operators ((<|^))
import Control.Monad.State

-- | A weaker variant of `At` that cannot be written to 
class AtConst m where
    (@) :: m -> Index m -> Maybe (IxValue m)

instance {-# OVERLAPPABLE #-} Ixed m => AtConst m where
    x @ i = execState ((ix i) (\y -> put (Just y) <|^ y) x) Nothing
