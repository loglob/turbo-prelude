# TurboPrelude
An alternate prelude and util library for Haskell.
Its primary feature is an unorthodox redefinition of a lot of Haskell's operators (see below).

## Including
To include the prelude, create a `cabal.project` file in your project root directory containing:
```yml
source-repository-package
    type: git
    location: https://github.com/loglob/turbo-prelude

packages: ./*.cabal
```
Then edit your `.cabal` file to include:
```yml
build-depends:
	base,
	turbo-prelude
mixins:
	base hiding (Prelude),
	turbo-prelude (Turbo.Prelude as Prelude, Control.Attempt, Control.Mapper, Control.SpanBuilder, Data.Or, Data.LargeText, Data.RList, Data.SignedSet, Data.Span)
```
I strongly recommend also adding these extensions:
```yml
default-extensions:
	BlockArguments
	LambdaCase
	TupleSections
```
or using the `GHC2021` language standard:
```yml
default-language: GHC2021
default-extensions:
	BlockArguments
```

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
Each of these also has variants with `$$`, `$$$` or `$$$$` in place of `$`.

The defined operator families are:
- Extensions for `.` with a higher-arity right function
	- Since `..` is reserved internally, its higher-order forms are `.:`, `.:.` and `.::`
- Picking operators `|^` and `^|` which discard the left and right values, respectively
	- These alias the hidden class members `<$`, `<*` and `*>`
	- Note that `<<` and `>>` are still exported since `<|^>` and `<^|>` are specifically for `Applicative`
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
- `@ :: AtConst a => a -> Index a -> Maybe (IxValue a)` generalizing `!?`
    - `AtConst` is a subclass of `At` from `Control.Lens`
- `@~ :: AtConstRev xs x => xs -> Int -> Maybe x` variant of `@` that indexes from the end
    - `AtConstRev` is a subclass of `Unsnoc`, which is a subclass of `Snoc`
- `°` function substitution that generalizes function composition
	- Substitutes its right argument for the last argument of its left argument 
	- Add `.` to the left or right to specify the arity of that function
	- `°`, `°.`, `°:`, etc. are exactly function composition `.`, `.:`, `.:.` etc.
- `°´` (that's a forward tick, not a backtick) function substitution where types coincide
	- i.e. the right argument is a function that accepts the same initial argument(s) as the left side, and produces the remaining argument(s), possibly as a tuple
	- the result is a function that only takes the arguments both sides have in common
	- dots to the left indicate the number of shared arguments, to the right the number of arguments the right side produces
	- the forward tick comes after the but before the angles, i.e. `.°:´` and `<..°.´>`
- The default `:` and `:|` constructors
	- These use `>:>` and `>:|>` instead of `:>` and `:|>` as those are illegal operator names
	- Wrappers for `++` are also included
- `?? :: Maybe a -> Maybe a -> Maybe a` and `?! :: Maybe a -> a -> a` coalescing operators
	- They are lazy in their right arguments and their functor and applicator wrappers also shortcut

With functor and applicative variants for each.
