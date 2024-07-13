module Turbo.Extra.Monad where
import Turbo.RootPrelude
import Turbo.Operators


-- | Applies the same computation on both sides of a Bifunctor
bothM :: (Bitraversable f, Applicative g) => (a -> g b) -> f a a -> g (f b b)
bothM f = bimapM f f

-- | Executes a computation and discards the result
btw :: (Functor m) => (a -> m ()) -> a -> m a
btw f a = f a <|^ a

-- | Alias for bitraverse only over the first value
firstM :: (Bitraversable f, Applicative g) => (a -> g b) -> f a x -> g (f b x)
firstM f = bitraverse f pure

-- | Alias for bitraverse only over the second value
secondM :: (Bitraversable f, Applicative g) => (a -> g b) -> f x a -> g (f x b)
secondM = bitraverse pure

-- | if-then-else lifted to a monad
ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM x t e =
    x >>= \case
        True -> t
        False -> e

-- | ifM that discards results
ifM_ :: (Monad m) => m Bool -> m a -> m b -> m ()
ifM_ x t e = ifM x (void t) (void e)

-- | Triple-nested variant of join
joinM :: (Monad m, Traversable n) => m (n (m a)) -> m (n a)
joinM = (>>= traverse id)

-- | Applies a computation and preserves its argument as the second value
toFstM :: (Functor f) => (a -> f b) -> a -> f (b, a)
toFstM f x = fmap (,x) (f x)

-- | Applies a computation and preserves its argument as the first value
toSndM :: (Functor f) => (a -> f b) -> a -> f (a, b)
toSndM f x = fmap (x,) (f x)

-- | unless lifted to monads preserving the result
unlessM :: (Monad m) => m Bool -> m a -> m (Maybe a)
unlessM x y = ifM x (return Nothing) (fmap Just y)

-- | unless lifted to monads discarding the result
unlessM_ :: (Monad m) => m Bool -> m a -> m ()
unlessM_ x y = ifM_ x (return ()) y

-- | Runs a computation only if a value is present
whenJust :: (Monad m) => Maybe a -> (a -> m (Maybe b)) -> m (Maybe b)
whenJust Nothing _ = return Nothing
whenJust (Just x) f = f x

whenJust' :: (Monad m) => Maybe a -> (a -> m b) -> m (Maybe b)
whenJust' Nothing _ = return Nothing
whenJust' (Just x) f = Just `fmap` f x

-- | Variant of `whenJust` that discards the result
whenJust_ :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust_ Nothing _ = return ()
whenJust_ (Just x) f = void (f x)

-- | `whenJust` inside a monad
whenJustM :: (Monad m) => m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
whenJustM x f = x >>= \y -> whenJust y f

-- | `whenJust'` inside a monad
whenJustM' :: (Monad m) => m (Maybe a) -> (a -> m b) -> m (Maybe b)
whenJustM' x f = x >>= \y -> whenJust' y f

whenJustM_ :: (Monad m) => m (Maybe a) -> (a -> m b) -> m ()
whenJustM_ x f = x >>= \y -> whenJust_ y f

-- | Runs a computation only if no value is present
whenNothing :: (Monad m) => Maybe a -> m b -> m (Maybe b)
whenNothing Nothing f = fmap Just f
whenNothing (Just _) _ = pure Nothing

-- | Runs a computation only if no value is present, discarding result
whenNothing_ :: (Monad m) => Maybe a -> m b -> m ()
whenNothing_ Nothing f = void f
whenNothing_ (Just _) _ = pure ()

whenNothingM :: (Monad m) => m (Maybe a) -> m b -> m (Maybe b)
whenNothingM x f = x >>= (`whenNothing` f)

whenNothingM_ :: (Monad m) => m (Maybe a) -> m b -> m ()
whenNothingM_ x f = x >>= (`whenNothing_` f)

-- | Variant of `ifM` with only one branch
whenM :: (Monad m) => m Bool -> m a -> m (Maybe a)
whenM x f = ifM x (fmap Just f) (return Nothing)

-- | Variant of `ifM_` with only one branch
whenM_ :: (Monad m) => m Bool -> m a -> m ()
whenM_ x f = ifM x (void f) (return ())

-- | Repeatedly computes a maybe until it becomes `Nothing`
whileJust :: (Monad m) => m (Maybe a) -> m [a]
whileJust f =
    f >>= \case
        Nothing -> return []
        Just x -> (x :) `fmap` whileJust f

-- | Repeats a computation until it outputs `False`
whileM :: (Monad m) => m Bool -> m ()
whileM c =
    c >>= \case
        True -> whileM c
        False -> return ()

-- | Variant of `whileM` that repeats until `True` is returned
untilM :: (Monad m) => m Bool -> m ()
untilM = whileM . fmap not

-- | concatMap lifted to monads
concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = m
  where
    m [] = return []
    m (x : xs) = f x <++> m xs
