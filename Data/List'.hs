module Data.List' where

import Control.Lens
import GHC.IsList as L
import Turbo.Prelude

infixl 5 :!

-- | A left-associative list
data List' a
    = -- | The empty list
      Nil
    | -- | Append to the right
      List' a :! a
    deriving (Eq, Ord, Functor)

-- | Converts an `RList` to a normal list and reverses it
toRevList :: List' a -> [a]
{-# INLINE toRevList #-}
toRevList Nil = []
toRevList (xs :! x) = x : toRevList xs

-- | Converts a normal List to an `RList` and reverses it
fromRevList :: [a] -> List' a
{-# INLINE fromRevList #-}
fromRevList [] = Nil
fromRevList (x : xs) = fromRevList xs :! x

instance IsList (List' a) where
    type Item (List' a) = a
    fromList :: [a] -> List' a
    fromList = fromRevList . reverse
    toList :: List' a -> [a]
    toList = reverse . toRevList

instance IsString (List' Char) where
    fromString :: String -> List' Char
    fromString = fromList

instance Semigroup (List' a) where
    (<>) :: List' a -> List' a -> List' a
    xs <> Nil = xs
    xs <> (ys :! y) = (xs <> ys) :! y

instance Monoid (List' a) where
    mempty :: List' a
    mempty = Nil

instance Cons (List' a) (List' a) a a where
    _Cons :: Prism' (List' a) (a, List' a)
    _Cons = prism' (uncurry f) g
      where
        f :: a -> List' a -> List' a
        f x Nil = Nil :! x
        f x (ys :! y) = f x ys :! y
        g :: List' a -> Maybe (a, List' a)
        g Nil = Nothing
        g (xs :! x) = Just $ g' xs x
        g' :: List' a -> a -> (a, List' a)
        g' Nil x = (x, Nil)
        g' (ys :! y) x = second (:! x) (g' ys y)

instance Snoc (List' a) (List' a) a a where
    _Snoc :: Prism' (List' a) (List' a, a)
    _Snoc = prism' (uncurry (:!)) \case
        Nil -> Nothing
        xs :! x -> Just (xs, x)

instance AsEmpty (List' a) where
    _Empty :: Prism' (List' a) ()
    _Empty = prism' (\() -> Nil) \case
        Nil -> Just ()
        _ -> Nothing

instance (Show a) => Show (List' a) where
    showsPrec n = showsPrec n . reverse . toRevList

instance Foldable List' where
    foldl :: (b -> a -> b) -> b -> List' a -> b
    foldl _ b Nil = b
    foldl f b (xs :! x) = foldl f b xs `f` x
    foldr :: (a -> b -> b) -> b -> List' a -> b
    foldr _ b Nil = b
    foldr f b (xs :! x) = foldr f (f x b) xs
    toList :: List' a -> [a]
    toList = L.toList
    null :: List' a -> Bool
    null Nil = True
    null _ = False
type instance Index (List' a) = Int
type instance IxValue (List' a) = a

instance Ixed (List' a) where
    ix i f xs =
        let
            i' = (if i < 0 then negate else (length xs -)) i
         in
            back i' xs
      where
        back _ Nil = pure Nil
        back 1 (xs :! x) = (xs :!) $> f x
        back i (xs :! x) = back (i - 1) xs <§ (:! x)

instance Applicative List' where
    pure :: a -> List' a
    pure = (Nil :!)
    liftA2 :: (a -> b -> c) -> List' a -> List' b -> List' c
    liftA2 _ __ Nil = Nil
    liftA2 f xs (ys :! y) = liftA2 f xs ys <> fmap (`f` y) xs

instance Alternative List' where
    empty :: List' a
    empty = Nil
    (<|>) :: List' a -> List' a -> List' a
    (<|>) = (<>)

instance Monad List' where
    (>>=) :: List' a -> (a -> List' b) -> List' b
    Nil >>= _ = Nil
    (xs :! x) >>= f = (xs >>= f) <> f x

instance MonadPlus List'
