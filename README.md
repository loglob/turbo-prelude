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
	turbo-prelude (Turbo.Prelude as Prelude, Control.Attempt, Control.Mapper, Data.Or, Data.LargeText, Data.RList, Data.SignedSet, Data.Span)
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

You can also use my [project template](https://github.com/loglob/scaffolds) which has this configuration already baked-in.

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

## Extra Datatypes
**Data.LargeText** provides a wrapper around `Data.Text` that provides efficient O(1) indexing as well as mapping to/from line/column positions.

**Data.Or** is a variant of `Data.Either` that permits both cases to be present at once.

**Data.RList** is a Prelude-like linked-list that appends to the right rather than prepending to the left.

**Data.SignedSet** is a (non-traversable) set that may be infinite, and can be defined via difference from the universe of a type.

### Data.Span
This module contains types for efficient O(1) slicing of data structures such as arrays and `Data.Text`.

**Data.Span.Span** is the type class that describes all such types.

**Data.Span.ArraySpan** is a span wrapper for `Array#` and `SmallArray#`

**Data.Span.ArraySpan** is a span wrapper for `MutableArray#` and `SmallMutableArray#`

**Data.Span.USpan** is a span wrapper for unboxed arrays. These are more efficient but don't permit laziness.

**Data.Span.ArraySpan** is a span wrapper for mutable unboxed arrays.

## Extra Type Classes

**Data.FoldableR** expresses that a type supports fold operations.
This class exists to allow folding for types that can't be `Foldable`, i.e. aren't `* -> *`, such as instances of `Uncons` or `Unsnoc`.  
Instances are e.g. all `Foldable`s, `Text`, `LargeText`, ...

**Data.ISpan** expresses that a type behaves like an array slice i.e. allows 0-copy O(1) slicing.  
Instances are e.g. `Text`, `LargeText`, `Data.Vector`, primitive arrays (via `Data.Span` or `Data.MutSpan`)

### Turbo.Cast
This module contains a set of classes for using both boxed and unboxed bit-fields.

**Boxed** expresses the relation between a lifted box type and an unboxed type.  
Instances are e.g. `Int` and `Int#`, `Char` and `Char#`, ...

**(:>:)** expresses that one bit-field type is wider than another bit-field type, with a set of casting operation.  
Instances are e.g. `Int64` and `Int32`, `Int64#` and `Int32#`, ...

**IsSigned** expresses that one bit-field type is the signed version of another bit-field type.
Instances are e.g. `Int64` and `Word64`, `Int32#` and `Word32#`, ...

### lens Extensions
These classes are weaker versions of classes defined in `lens`.

**Turbo.Prelude.Uncons** is weaker `Cons` that expresses that a type can be recursively destructured into a leftmost item and a tail.  
Instances are e.g. `[]`.  
This class is used to provide generalized variants of list operations, such as `Turbo.Prelude.dropWhile`.

**Turbo.Prelude.Unsnoc** is weaker `Snoc` that expresses that a type can be recursively destructured into a rightmost item and a prefix.  
Instances are e.g. `RList`

**Turbo.Prelude.AtConst** is weaker `At` that expresses that a type can be indexed to produce values (but does not provide lenses for those items).  
This class is used to express the preferred indexing operator `@`.  
Instances are e.g. `[]`, `RList`, `Map`

**Turbo.Prelude.AtConstRev** is a variant of `AtConst` that permits indexing from right to left. Its indices must be `Int`s.
It is used to express the reversed indexing operator `@~`.  
Instances are e.g. `[]`, `RList`, ...
-+