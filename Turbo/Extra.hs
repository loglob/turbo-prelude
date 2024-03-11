module Turbo.Extra where
import Turbo.RootPrelude
import qualified Data.Foldable as F
import qualified Data.Functor as Fu
import qualified Data.Text as T
import Turbo.Operators ((<++>), (>:|>), (<:|))

-- * Misc functions
-- | Prepends a text's contents before a suffix string
unpacks :: Text -> String -> String
unpacks t x = case T.uncons t of
    Nothing    -> x
    Just (c,r) -> c : unpacks r x

-- | Access the left element of an Either
left :: Either a b -> Maybe a
left (Left  a) = Just a
left (Right _) = Nothing

-- | Access the right element of an Either
right :: Either a b -> Maybe b
right (Left  _) = Nothing
right (Right b) = Just b

-- | Applies a function and preserves its argument as the second value
toFst :: (a -> b) -> a -> (b,a)
toFst f x = (f x,x)

-- | Applies a function and preserves its argument as the first value
toSnd :: (a -> b) -> a -> (a,b)
toSnd f x = (x,f x)

-- ** List Operations
-- | Lens for the head of a NonEmpty
_head1 :: Lens' (NonEmpty a) a
_head1 f (x :| xs)= f x <:| xs

-- | Lens for the possibly-empty tail of a NonEmpty
_tail1 :: Lens' (NonEmpty a) [a]
_tail1 f (x :| xs) = x >:|> f xs

-- | Lens for the head of a NonEmpty
_last1 :: Lens' (NonEmpty a) a
_last1 f (x :| xs) = g x xs where
    g x    []  = f x <:| []
    g x (y:ys) = fmap (x <|)  (g y ys)

-- | concatMap lifted to monads
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f = m where
    m    []  = return []
    m (x:xs) = f x <++> m xs

-- | Splits a list by a predicate
split :: (a -> Bool) -> [a] -> NonEmpty [a]
split f = spl where
    spl    []  = [] :| []
    spl (x:xs) = if f x
                    then [] <| split f xs
                    else let y :| ys = spl xs in (x : y) :| ys

-- | Trims the left and right sides of a list by some removal predicate
trim :: (a -> Bool) -> [a] -> [a]
trim f = dropWhileEnd f .dropWhile f

-- ** Monad Operations
-- | Executes a computation and discards the result
btw :: Functor m => (a -> m ()) -> a -> m a
btw f a = f a Fu.$> a

-- | if-then-else lifted to a monad
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM x t e = x >>= \case
    True  -> t
    False -> e

-- | ifM that discards results
ifM_ :: Monad m => m Bool -> m a -> m b -> m ()
ifM_ x t e = ifM x (void t) (void e)

-- | Triple-nested variant of join
joinM :: (Monad m, Traversable n) => m (n (m a)) -> m (n a)
joinM = (>>= traverse id)

-- | Applies a computation and preserves its argument as the second value
toFstM :: Functor f => (a -> f b) -> a -> f (b,a)
toFstM f x = fmap (,x) (f x)

-- | Applies a computation and preserves its argument as the first value
toSndM :: Functor f => (a -> f b) -> a -> f (a,b)
toSndM f x = fmap (x,) (f x)

-- | unless lifted to monads preserving the result
unlessM :: Monad m => m Bool -> m a -> m (Maybe a)
unlessM x y = ifM x (return Nothing) (fmap Just y)

-- | unless lifted to monads discarding the result
unlessM_ :: Monad m => m Bool -> m a -> m ()
unlessM_ x y = ifM_ x (return()) y

-- | Runs a computation only if a value is present
whenJust :: Monad m => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJust Nothing  _ = return Nothing
whenJust (Just x) f = f x

-- | Variant of `whenJust` that discards the result
whenJust_ :: Monad m => Maybe a -> (a -> m b) -> m ()
whenJust_ Nothing  _ = return ()
whenJust_ (Just x) f = void (f x)

-- | `whenJust` inside a monad
whenJustM :: Monad m => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustM x f = x >>= \y -> whenJust y f

whenJustM_ :: Monad m => m (Maybe a) -> (a -> m b) -> m ()
whenJustM_ x f = x >>= \y -> whenJust_ y f

-- | Variant of `ifM` with only one branch
whenM :: Monad m => m Bool -> m a -> m (Maybe a)
whenM x f = ifM x (fmap Just f) (return Nothing)

-- | Variant of `ifM_` with only one branch
whenM_ :: Monad m => m Bool -> m a -> m ()
whenM_ x f = ifM x (void f) (return ())

-- | Repeatedly computes a maybe until it becomes `Nothing`
whileJust :: Monad m => m (Maybe a) -> m [a]
whileJust f = f >>= \case
    Nothing -> return []
    Just x  -> (x:) `fmap` whileJust f

-- | Repeats a computation until it outputs `False`
whileM :: Monad m => m Bool -> m ()
whileM c = c >>= \case
    True -> whileM c
    False -> return ()

-- | Variant of `whileM` that repeats until `True` is returned
untilM :: Monad m => m Bool -> m ()
untilM = whileM .fmap not


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
forkM :: Applicative f => (a -> f x) -> (a -> f y) -> a -> f (x,y)
forkM f g a = liftA2 (,) (f a) (g a)
-- | Sends two values to two computations
fork2M :: Applicative f => (a -> b -> f x) -> (a -> b -> f y) -> a -> b -> f (x, y)
fork2M f g a b = liftA2 (,) (f a b) (g a b)
-- | Sends three values to two computations
fork3M :: Applicative f => (a -> b -> c -> f x) -> (a -> b -> c -> f y) -> a -> b -> c -> f (x, y)
fork3M f g a b c = liftA2 (,) (f a b c) (g a b c)
-- | Sends four values to two computations
fork4M :: Applicative f => (a -> b -> c -> d -> f x) -> (a -> b -> c -> d -> f y) -> a -> b -> c -> d -> f (x, y)
fork4M f g a b c d = liftA2 (,) (f a b c d) (g a b c d)


-- * three-branch forking operator
-- | Sends a single value to three functions
triFork :: (a -> x) -> (a -> y) -> (a -> z) -> a -> (x,y,z)
triFork f g h a = (f a, g a, h a)

-- | Sends two values to three functions
triFork2 :: (a -> b -> x) -> (a -> b -> y) -> (a -> b -> z) -> a -> b -> (x,y,z)
triFork2 f g h a b = (f a b, g a b, h a b)

-- | Sends three values to three functions
triFork3 :: (a -> b -> c -> x) -> (a -> b -> c -> y) -> (a -> b -> c -> z) -> a -> b -> c -> (x,y,z)
triFork3 f g h a b c = (f a b c, g a b c, h a b c)

-- | Sends four values to three functions
triFork4 :: (a -> b -> c -> d -> x) -> (a -> b -> c -> d -> y) -> (a -> b -> c -> d -> z) -> a -> b -> c -> d -> (x,y,z)
triFork4 f g h a b c d = (f a b c d, g a b c d, h a b c d)

-- ** Monadic variants
-- | Sends a value to three computations
triForkM :: Applicative f => (a -> f x) -> (a -> f y) -> (a -> f z) -> a -> f (x,y,z)
triForkM f g h a = liftA3 (,,) (f a) (g a) (h a)

-- | Sends two values to three computations
triFork2M :: Applicative f => (a -> b -> f x) -> (a -> b -> f y) -> (a -> b -> f z) -> a -> b -> f (x,y,z)
triFork2M f g h a b = liftA3 (,,) (f a b) (g a b) (h a b)

-- | Sends three values to three computations
triFork3M :: Applicative f => (a -> b -> c -> f x) -> (a -> b -> c -> f y) -> (a -> b -> c -> f z) -> a -> b -> c -> f (x,y,z)
triFork3M f g h a b c = liftA3 (,,) (f a b c) (g a b c) (h a b c)

-- | Sends four values to three computations
triFork4M :: Applicative f => (a -> b -> c -> d -> f x) -> (a -> b -> c -> d -> f y) -> (a -> b -> c -> d -> f z) -> a -> b -> c -> d -> f (x,y,z)
triFork4M f g h a b c d = liftA3 (,,) (f a b c d) (g a b c d) (h a b c d)


-- * Replacers for partial functions
-- ** empty-safe foldable functions
-- | Counts the number of elements that fulfill a predicate
count :: (Foldable f, Num i) => (a -> Bool) -> f a -> i
count f = sumBy (\x -> if f x then 1 else 0)

-- | Retrieves the maximum value, if any
maximum :: (Foldable f, Ord a) => f a -> Maybe a
maximum xs = if F.null xs then Nothing else Just (F.maximum xs)

-- | Retrieves the maximum value, if any, by the given ordering function.
--   If there are multiple such values, returns the leftmost one.
maximumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
maximumBy f xs = foldr g Nothing xs where
    g x Nothing = Just x
    g x (Just y) = Just case f x y of
        GT -> x
        EQ -> x
        LT -> y

-- | Retrieves the minimum value, if any
minimum :: (Foldable f, Ord a) => f a -> Maybe a
minimum xs = if F.null xs then Nothing else Just (F.minimum xs)

-- | Retrieves the minimum value, if any, by the given ordering function.
--   If there are multiple such values, returns the leftmost one.
minimumBy :: Foldable f => (a -> a -> Ordering) -> f a -> Maybe a
minimumBy f xs = foldr g Nothing xs where
    g x Nothing = Just x
    g x (Just y) = Just case f x y of
        GT -> y
        EQ -> x
        LT -> x

-- | Sums a foldable under a mapping
sumBy :: (Foldable f, Num n) => (a -> n) -> f a -> n
sumBy f = foldl' (\x y -> x + f y) 0

-- | Lifts a function over `NonEmpty` to lists
viaNE :: (NonEmpty a -> b) -> [a] -> Maybe b
viaNE _ [] =  Nothing
viaNE f (x:xs) = Just$ f (x :| xs)
