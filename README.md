# TurboHaskell
An alternate prelude and util library for Haskell.
Its primary feature is an unorthodox redefinition of a lot of Haskell's operators (see below).

## Custom Operators
The prelude redefines a lot of Haskell's default operators.
This is mostly for ironing out inconsistencies in the default Prelude, i.e. why is `<$` included while `$>` is not.
The operators' syntax also aims to make an actual connection between how an operator looks and what it does.
The principles behind this are:
- `<` and `>` mark a side of a binary operator that accept an argument within a `Functor` type
	- If both sides are marked, an `Applicative` is required
- Reduplication of the operator itself implies some deeper nesting of the argument type
	- i.e. `$$` is a variant of `$` that applies an arity-2 function on a pair of values

For example, `$` has these unwrapped variants:
```hs
-- All of these have the same fixity as $
($$)   :: (a -> b -> c) -> (a,b) -> c
($$$)  :: (a -> b -> c -> d) -> (a,b,c) -> d
($$$$) :: (a -> b -> c -> d -> e) -> (a,b,c,d) -> e
```
With these wrapped variant **which overwrite default definitions**:
```hs
(<$)  :: Functor f => f (a -> b) -> a -> f b
($>)  :: Functor f => (a -> b) -> f a -> f b
(<$>) :: Applicative f => f (a -> b) -> f a -> f b
```
These also have higher arity wrappers:
```hs
(<<$)  :: Functor f => (x -> f (a -> b)) -> a -> x -> f b
($>>)  :: Functor f => (a -> b) -> (x -> f a) -> x -> f b
(<<$>) :: Applicative f => (x -> f (a -> b)) -> f a -> x -> f b
(<$>>) :: Applicative f => f (a -> b) -> (x -> f a) -> x -> f b
```
And variants for higher-arity functions, `<<<$`, `$>>>`, `<<<$>`, `<$>>>`, `<<<<$`, `$>>>>`, `<<<<$>` and `<$>>>>`.
Each of these also has variants with `$$`, `$$$` or `$$$$` in place of `$`.

The defined operator families are:
- Extensions for `.`
	- Since `..` is reserved internally, its higher-order forms are `.:`, `.:.` and `.::`
- Picking operators `|^` and `^|` which discard the left and right values, respectively
	- These alias the hidden class members `<$`, `<*` and `*>`
	- Note that `<<` and `>>` are completely hidden
- `§ :: a -> (a -> b) -> b` as flipped `$`
	- Also `§§`, `§§§` and `§§§§` analogous to `$$`, `$$$` and `$$$$`
	- Note that `<&>` is overwritten, use `<§` instead
- `& :: a -> b -> (a,b)` for tuple merging
	- Instead of duplication, it uses `.` for its higher order variants
	- The number of dots gives the number of elements in that side's tuple, minus 1
	- i.e. `.& :: (a,b) -> c -> (a,b,c)`
	- or `.&.. :: (a,b) -> (c,d,e) -> (a,b,c,d,e)`
- `~ :: (a -> b) -> (b -> c) -> a -> c` as flipped `.`
	- This has no duplicated forms
- The default `:` and `:|` constructors
	- These use `>:>` and `>:|>` instead of `:>` and `:|>` as those are illegal operator names
	- Wrappers for `++` are also included
- `?? :: Maybe a -> Maybe a -> Maybe a` and `?! :: Maybe a -> a -> a` coalescing operators
	- They are lazy in their right arguments and their functor and applicator wrappers also shortcut

With functor and applicative variants for each.