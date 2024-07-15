module Data.RList where

import Control.Lens
import Data.Foldable qualified
import GHC.IsList as L
import Turbo.Prelude

infixl 5 :!

-- | A right-appending, left-associative list
data RList a
    = -- | The empty list
      Nil
    | -- | Append to the right
      RList a :! a
    deriving (Eq, Ord, Functor, Traversable)

-- | Converts an `RList` to a normal list and reverses it
toRevList :: RList a -> [a]
{-# INLINE toRevList #-}
toRevList Nil = []
toRevList (xs :! x) = x : toRevList xs

-- | Converts a normal List to an `RList` and reverses it
fromRevList :: [a] -> RList a
{-# INLINE fromRevList #-}
fromRevList [] = Nil
fromRevList (x : xs) = fromRevList xs :! x

instance IsList (RList a) where
    type Item (RList a) = a
    fromList :: [a] -> RList a
    fromList = fromRevList . reverse
    toList :: RList a -> [a]
    toList = reverse . toRevList

instance IsString (RList Char) where
    fromString :: String -> RList Char
    fromString = fromList

instance Semigroup (RList a) where
    (<>) :: RList a -> RList a -> RList a
    xs <> Nil = xs
    xs <> (ys :! y) = (xs <> ys) :! y

instance Monoid (RList a) where
    mempty :: RList a
    mempty = Nil

instance Cons (RList a) (RList a) a a where
    _Cons :: Prism' (RList a) (a, RList a)
    _Cons = prism' (uncurry f) g
      where
        f :: a -> RList a -> RList a
        f x Nil = Nil :! x
        f x (ys :! y) = f x ys :! y
        g :: RList a -> Maybe (a, RList a)
        g Nil = Nothing
        g (xs :! x) = Just $ g' xs x
        g' :: RList a -> a -> (a, RList a)
        g' Nil x = (x, Nil)
        g' (ys :! y) x = second (:! x) (g' ys y)

instance Snoc (RList a) (RList a) a a where
    _Snoc :: Prism' (RList a) (RList a, a)
    _Snoc = prism' (uncurry (:!)) \case
        Nil -> Nothing
        xs :! x -> Just (xs, x)

instance AsEmpty (RList a) where
    _Empty :: Prism' (RList a) ()
    _Empty = prism' (\() -> Nil) \case
        Nil -> Just ()
        _ -> Nothing

instance (Show a) => Show (RList a) where
    showsPrec n = showsPrec n . reverse . toRevList

instance Foldable RList where
    foldl :: (b -> a -> b) -> b -> RList a -> b
    foldl _ b Nil = b
    foldl f b (xs :! x) = foldl f b xs `f` x
    foldr :: (a -> b -> b) -> b -> RList a -> b
    foldr _ b Nil = b
    foldr f b (xs :! x) = foldr f (f x b) xs
    toList :: RList a -> [a]
    toList = L.toList
    null :: RList a -> Bool
    null Nil = True
    null _ = False
type instance Index (RList a) = Int
type instance IxValue (RList a) = a

instance Ixed (RList a) where
    ix i f xs =
        let
            i' = (if i < 0 then negate else (length xs -)) i
         in
            back i' xs
      where
        back _ Nil = pure Nil
        back 1 (xs :! x) = (xs :!) $> f x
        back i (xs :! x) = back (i - 1) xs <§ (:! x)

instance Applicative RList where
    pure :: a -> RList a
    pure = (Nil :!)
    liftA2 :: (a -> b -> c) -> RList a -> RList b -> RList c
    liftA2 _ __ Nil = Nil
    liftA2 f xs (ys :! y) = liftA2 f xs ys <> fmap (`f` y) xs

instance Alternative RList where
    empty :: RList a
    empty = Nil
    (<|>) :: RList a -> RList a -> RList a
    (<|>) = (<>)

instance Monad RList where
    (>>=) :: RList a -> (a -> RList b) -> RList b
    Nil >>= _ = Nil
    (xs :! x) >>= f = (xs >>= f) <> f x

instance MonadPlus RList
