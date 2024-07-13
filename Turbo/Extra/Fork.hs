module Turbo.Extra.Fork where
import Turbo.RootPrelude

-- * two-branch forking operator

-- | Sends a single value to two functions
fork :: (a -> x) -> (a -> y) -> a -> (x, y)
fork f g a = (f a, g a)

-- | Sends two values to two functions
fork2 :: (a -> b -> x) -> (a -> b -> y) -> a -> b -> (x, y)
fork2 f g a b = (f a b, g a b)

-- | Sends three values to two functions
fork3 :: (a -> b -> c -> x) -> (a -> b -> c -> y) -> a -> b -> c -> (x, y)
fork3 f g a b c = (f a b c, g a b c)

-- | Sends four values to two functions
fork4 :: (a -> b -> c -> d -> x) -> (a -> b -> c -> d -> y) -> a -> b -> c -> d -> (x, y)
fork4 f g a b c d = (f a b c d, g a b c d)

-- ** Monadic variants

-- | Sends a single value to two computations
forkM :: (Applicative f) => (a -> f x) -> (a -> f y) -> a -> f (x, y)
forkM f g a = liftA2 (,) (f a) (g a)

-- | Sends two values to two computations
fork2M :: (Applicative f) => (a -> b -> f x) -> (a -> b -> f y) -> a -> b -> f (x, y)
fork2M f g a b = liftA2 (,) (f a b) (g a b)

-- | Sends three values to two computations
fork3M :: (Applicative f) => (a -> b -> c -> f x) -> (a -> b -> c -> f y) -> a -> b -> c -> f (x, y)
fork3M f g a b c = liftA2 (,) (f a b c) (g a b c)

-- | Sends four values to two computations
fork4M :: (Applicative f) => (a -> b -> c -> d -> f x) -> (a -> b -> c -> d -> f y) -> a -> b -> c -> d -> f (x, y)
fork4M f g a b c d = liftA2 (,) (f a b c d) (g a b c d)

-- * three-branch forking operator

-- | Sends a single value to three functions
triFork :: (a -> x) -> (a -> y) -> (a -> z) -> a -> (x, y, z)
triFork f g h a = (f a, g a, h a)

-- | Sends two values to three functions
triFork2 :: (a -> b -> x) -> (a -> b -> y) -> (a -> b -> z) -> a -> b -> (x, y, z)
triFork2 f g h a b = (f a b, g a b, h a b)

-- | Sends three values to three functions
triFork3 :: (a -> b -> c -> x) -> (a -> b -> c -> y) -> (a -> b -> c -> z) -> a -> b -> c -> (x, y, z)
triFork3 f g h a b c = (f a b c, g a b c, h a b c)

-- | Sends four values to three functions
triFork4 :: (a -> b -> c -> d -> x) -> (a -> b -> c -> d -> y) -> (a -> b -> c -> d -> z) -> a -> b -> c -> d -> (x, y, z)
triFork4 f g h a b c d = (f a b c d, g a b c d, h a b c d)
-- ** Monadic variants

-- | Sends a value to three computations
triForkM :: (Applicative f) => (a -> f x) -> (a -> f y) -> (a -> f z) -> a -> f (x, y, z)
triForkM f g h a = liftA3 (,,) (f a) (g a) (h a)

-- | Sends two values to three computations
triFork2M :: (Applicative f) => (a -> b -> f x) -> (a -> b -> f y) -> (a -> b -> f z) -> a -> b -> f (x, y, z)
triFork2M f g h a b = liftA3 (,,) (f a b) (g a b) (h a b)

-- | Sends three values to three computations
triFork3M :: (Applicative f) => (a -> b -> c -> f x) -> (a -> b -> c -> f y) -> (a -> b -> c -> f z) -> a -> b -> c -> f (x, y, z)
triFork3M f g h a b c = liftA3 (,,) (f a b c) (g a b c) (h a b c)

-- | Sends four values to three computations
triFork4M :: (Applicative f) => (a -> b -> c -> d -> f x) -> (a -> b -> c -> d -> f y) -> (a -> b -> c -> d -> f z) -> a -> b -> c -> d -> f (x, y, z)
triFork4M f g h a b c d = liftA3 (,,) (f a b c d) (g a b c d) (h a b c d)
