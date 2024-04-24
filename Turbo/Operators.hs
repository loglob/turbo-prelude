{-# OPTIONS_GHC -Wno-missing-signatures #-}

-- | Non-standard but internally consistent operator syntax for functors/applicative/monads
module Turbo.Operators where

import Control.Applicative qualified as A
import Control.Monad qualified as M
import Data.Functor qualified as F
import GHC.Base (seq)
import Turbo.OperatorsTH
import Turbo.RootPrelude

-- * \$-family
infixr 0 $$, $$$, $$$$
{-# INLINE ($$) #-}
{-# INLINE ($$$) #-}
{-# INLINE ($$$$) #-}
f $$ (a, b) = f a b
f $$$ (a, b, c) = f a b c
f $$$$ (a, b, c, d) = f a b c d

makeOperators '($)
makeOperators '($$)
makeOperators '($$$)
makeOperators '($$$$)

-- ** Postponing variants
makePostponed 1 [1 .. 5]
makePostponed 2 [1 .. 5]
makePostponed 3 [1 .. 5]
makePostponed 4 [1 .. 5]

-- ** strict variants
infixr 0 $$!, $$$!, $$$$!
{-# INLINE ($$!) #-}
{-# INLINE ($$$!) #-}
{-# INLINE ($$$$!) #-}

-- | Strict variant of `$$` that ensures each argument is in WHNF before applying `f`
f $$! (a, b) = let !a' = a in let !b' = b in f a' b'

-- | Strict variant of `$$$` that ensures each argument is in WHNF before applying `f`
f $$$! (a, b, c) = let !a' = a in let !b' = b in let !c' = c in f a' b' c'

-- | Strict variant of `$$$$` that ensures each argument is in WHNF before applying `f`
f $$$$! (a, b, c, d) = let !a' = a in let !b' = b in let !c' = c in let !d' = d in f a' b' c' d'

infixr 4 $!>, $$!>, $$$!>, $$$$!>
{-# INLINE ($!>) #-}
{-# INLINE ($$!>) #-}
{-# INLINE ($$$!>) #-}
{-# INLINE ($$$$!>) #-}

{- | Strict version of `$>`
 Note that these are strict in their return, whereas $! is strict in its arguments
-}
($!>) :: (Monad m) => (a -> b) -> m a -> m b
($!>) = (M.<$!>)

-- | Strict variant of `$$>`
f $$!> m = do
    (a, b) <- m
    let x = f a b
    x `seq` return x

-- | Strict variant of `$$$>`
f $$$!> m = do
    (a, b, c) <- m
    let x = f a b c
    x `seq` return x

-- | Strict variant of `$$$$>`
f $$$$!> m = do
    (a, b, c, d) <- m
    let x = f a b c d
    x `seq` return x

makeLeftWrapper '($!)
makeLeftWrapper '($$!)
makeLeftWrapper '($$$!)
makeLeftWrapper '($$$$!)

-- * §-family
infixl 1 §, §§, §§§, §§§§
(§) :: a -> (a -> b) -> b
(§§) :: (a, b) -> (a -> b -> c) -> c
(§§§) :: (a, b, c) -> (a -> b -> c -> d) -> d
(§§§§) :: (a, b, c, d) -> (a -> b -> c -> d -> e) -> e
{-# INLINE (§) #-}
{-# INLINE (§§) #-}
{-# INLINE (§§§) #-}
{-# INLINE (§§§§) #-}
(§) = flip ($)
(§§) = flip ($$)
(§§§) = flip ($$$)
(§§§§) = flip ($$$$)

makeOperators '(§)
makeOperators '(§§)
makeOperators '(§§§)
makeOperators '(§§§§)

-- ** Strict Variants
infixl 1 §!, §§!, §§§!, §§§§!
(§!) :: a -> (a -> b) -> b
(§§!) :: (a, b) -> (a -> b -> c) -> c
(§§§!) :: (a, b, c) -> (a -> b -> c -> d) -> d
(§§§§!) :: (a, b, c, d) -> (a -> b -> c -> d -> e) -> e
{-# INLINE (§!) #-}
{-# INLINE (§§!) #-}
{-# INLINE (§§§!) #-}
{-# INLINE (§§§§!) #-}
(§!) = flip ($!)
(§§!) = flip ($$!)
(§§§!) = flip ($$$!)
(§§§§!) = flip ($$$$!)

infixl 4 <§!, <§§!, <§§§!, <§§§§!
(<§!) :: (Monad m) => m a -> (a -> b) -> m b
(<§§!) :: (Monad m) => m (a, b) -> (a -> b -> c) -> m c
(<§§§!) :: (Monad m) => m (a, b, c) -> (a -> b -> c -> d) -> m d
(<§§§§!) :: (Monad m) => m (a, b, c, d) -> (a -> b -> c -> d -> e) -> m e
(<§!) = flip ($!>)
(<§§!) = flip ($$!>)
(<§§§!) = flip ($$$!>)
(<§§§§!) = flip ($$$$!>)

makeRightWrapper '(§!)
makeRightWrapper '(§§!)
makeRightWrapper '(§§§!)
makeRightWrapper '(§§§§!)

-- *  Extended .-family
infixr 9 .:

-- | Higher-order function composition for arity 2
(.:) :: (x -> y) -> (a -> b -> x) -> a -> b -> y
{-# INLINE (.:) #-}
(.:) f g a b = f (g a b)

infixr 9 .:.

-- | Higher-order function composition for arity 3
(.:.) :: (x -> y) -> (a -> b -> c -> x) -> a -> b -> c -> y
{-# INLINE (.:.) #-}
(.:.) f g a b c = f (g a b c)

infixr 9 .::

-- | Higher-order function composition for arity 4
(.::) :: (x -> y) -> (a -> b -> c -> d -> x) -> a -> b -> c -> d -> y
{-# INLINE (.::) #-}
(.::) f g a b c d = f (g a b c d)

infixr 9 .::.

-- | Higher-order function composition for arity 5
(.::.) :: (x -> y) -> (a -> b -> c -> d -> e -> x) -> (a -> b -> c -> d -> e -> y)
{-# INLINE (.::.) #-}
(.::.) f g a b c d e = f (g a b c d e)

makeOperators '(.)
makeOperators '(.:)
makeOperators '(.:.)
makeOperators '(.::)
makeOperators '(.::.)

-- * °-family of function substitution
makeFuncSubst 0
makeFuncSubst 1
makeFuncSubst 2
makeFuncSubst 3
makeFuncSubst 4
makeFuncSubst 5

-- * &-family of tuple mergers

-- ** pairs
makeTuplePaste 0
-- ** triples
makeTuplePaste 1
-- ** 4-tuples
makeTuplePaste 2
-- ** 5-tuples
makeTuplePaste 3
-- ** 6-tuples
makeTuplePaste 4

-- * picking operators
infixr 4 ^|, ^|>, <^|>
infixl 4 |^, <|^, <|^>

-- | Returns the first argument. Operator form of `const`
(^|) :: a -> b -> a

-- | Returns the second argument. Operator form of `flip const`
(|^) :: a -> b -> b

{- | Runs a computation only for its side effects, then replaces its return value.
  Alias for hidden `Functor` member `<$`
-}
(^|>) :: (Functor f) => a -> f b -> f a

-- | Runs a computation only for its side effects, then replaces its return value.
(<|^) :: (Functor f) => f a -> b -> f b

-- | Runs two computations left-to-right, then picks only the left return value
(<^|>) :: (Applicative f) => f a -> f b -> f a

-- | Runs two computations left-to-right, then picks only the right return value
(<|^>) :: (Applicative f) => f a -> f b -> f b

{-# INLINE (^|) #-}
{-# INLINE (|^) #-}
{-# INLINE (^|>) #-}
{-# INLINE (<|^) #-}
{-# INLINE (<^|>) #-}
{-# INLINE (<|^>) #-}
_ |^ b = b
a ^| _ = a

-- use builtin ops for performance
(^|>) = (F.<$)
(<|^) = (F.$>)
(<|^>) = (A.*>)
(<^|>) = (A.<*)

-- * ~-family of reversed composition
infixl 9 ~

-- | Reversed function composition
(~) :: (a -> b) -> (b -> c) -> a -> c
{-# INLINE (~) #-}
g ~ f = f . g

-- Can't use template due to "Illegal variable name: ‘~’"
infixl 4 <~
(<~) :: (Functor f) => f (a -> b) -> (b -> c) -> f (a -> c)
{-# INLINE (<~) #-}
g <~ f = fmap (~ f) g

infixl 4 ~>
(~>) :: (Functor f) => (a -> b) -> f (b -> c) -> f (a -> c)
{-# INLINE (~>) #-}
g ~> f = fmap (g ~) f

infixl 4 <~>
(<~>) :: (Applicative f) => f (a -> b) -> f (b -> c) -> f (a -> c)
{-# INLINE (<~>) #-}
(<~>) = liftA2 (~)

-- * List operators

-- ** :-family
infixr 5 >:>

{- | Prepends values to boxed lists
  `:` prefixes a constructor operator, so this breaks the convention
-}
(>:>) :: (Functor f) => a -> f [a] -> f [a]
{-# INLINE (>:>) #-}
a >:> x = fmap (a :) x

makeLeftWrapper '(:)
makeApplicativeWrapper '(:)

-- ** Nothing-eliminating `:` variant
infixr 5 ?:
(?:) :: Maybe a -> [a] -> [a]
{-# INLINE (?:) #-}
Nothing ?: xs = xs
Just x ?: xs = x : xs

makeOperators '(?:)

-- ** ++-family
makeOperators '(++)

-- ** :|-family
infixr 5 >:|>

{- | Prepends values to boxed lists
  `:` prefixes a constructor operator, so this breaks the convention
-}
(>:|>) :: (Functor f) => a -> f [a] -> f (NonEmpty a)
{-# INLINE (>:|>) #-}
a >:|> x = (a :|) $> x

makeLeftWrapper '(:|)
makeApplicativeWrapper '(:|)

-- * Maybe coalescence

-- ** Maybe eliminating
infixl 6 ?!, ?!>, <?!>

-- | Replaces `Nothing` with the given value
orElse, (?!) :: Maybe a -> a -> a
{-# INLINE orElse #-}
{-# INLINE (?!) #-}
orElse Nothing a = a
orElse (Just a) _ = a
(?!) = orElse

-- | Runs a computation only if no value is present already
orElseM, (?!>) :: (Applicative f) => Maybe a -> f a -> f a
{-# INLINE orElseM #-}
{-# INLINE (?!>) #-}
orElseM Nothing x = x
orElseM (Just x) _ = pure x
(?!>) = orElseM

{- | Runs a second computation only if the first produced no value

 Note that it requires a Monad. For an Applicative version that is eager in its second computation, use `liftA2 (?!)`
-}
(<?!>) :: (Monad m) => m (Maybe a) -> m a -> m a
{-# INLINE (<?!>) #-}
l <?!> r =
    l >>= \case
        Just x -> return x
        Nothing -> r

-- | Replaces a computation's result if it's `Nothing`
makeLeftWrapper '(?!)

-- ** Maybe preserving
infixl 6 ??, ??>, <??>

-- | Returns the leftmost `Just`
(??) :: Maybe a -> Maybe a -> Maybe a
{-# INLINE (??) #-}
Just x ?? _ = Just x
Nothing ?? y = y

-- | Runs a maybe-computation only if no value is present already
(??>) :: (Applicative f) => Maybe a -> f (Maybe a) -> f (Maybe a)
{-# INLINE (??>) #-}
Just x ??> _ = pure (Just x)
Nothing ??> x = x

{- | Runs a second computation only if the first produced no value

 Note that it requires a Monad for short-cutting.
 For an Applicative version that is eager in its second computation, use `liftA2 (??)`
-}
(<??>) :: (Monad f) => f (Maybe a) -> f (Maybe a) -> f (Maybe a)
{-# INLINE (<??>) #-}
x <??> y =
    x >>= \case
        Just a -> return (Just a)
        Nothing -> y

-- | Replaces a computation's result if it's `Nothing`
makeLeftWrapper '(??)

-- * Misc
infixr 3 &&&, |||

-- | Joins two predicate functions
(&&&) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
{-# INLINE (&&&) #-}
a &&& b = \x -> a x && b x

-- | Joins two predicate functions
(|||) :: (t -> Bool) -> (t -> Bool) -> t -> Bool
{-# INLINE (|||) #-}
a ||| b = \x -> a x || b x

infixl 7 /^

-- | Integer division rounding up
(/^) :: (Integral a) => a -> a -> a
a /^ b = signum (a `mod` b) + a `div` b

infixr 6 <>?

-- | Semigroup operation with one-sided maybe
(<>?) :: (Semigroup a) => a -> Maybe a -> a
x <>? Nothing = x
x <>? Just y = x <> y

infixl 6 ?<>

-- | Semigroup operation with one-sided maybe
(?<>) :: (Semigroup a) => Maybe a -> a -> a
Nothing ?<> y = y
Just x ?<> y = x <> y

infixl 1 >>~

-- | Result-ignoring variant of `>>=` (has nothing to do with the `~`-family)
(>>~) :: (Monad m) => m a -> (a -> m ()) -> m a
x >>~ f = x >>= \y -> f y <|^ y
