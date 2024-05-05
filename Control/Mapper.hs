-- | Monad for left-to-right (de-)construction
module Control.Mapper (
    Builder,
    BuilderS,
    BuilderT,
    Mapper,
    MapperS,
    MapperST,
    MapperT (),
    Reducer,
    ReducerT,
    getLastSpan,
    peek,
    peekSpan,
    pop,
    popWhen,
    runBuilder,
    runBuilderS,
    runBuilderT,
    runMapper,
    runMapperS,
    runMapperST,
    runMapperT,
    runReducer,
    runReducerT,
    trace,
    trace',
    yield,
    yield',
    yieldS,
    yieldS',
) where

import Control.Monad.State
import Control.Monad.Trans.State (modifyM)
import Data.Primitive (Prim)
import Data.Span
import GHC.Err (error)
import Turbo.Prelude

-- * Types

-- ** Internal

data MapperState b r = S
    { -- | The remaining input
      rest :: r,
      -- | The current output
      _output :: b
    }

makeLenses ''MapperState

-- ** Exposed

{- | A mapper that processes an input from left to right, while building an output and returning a value.
  Operates as a monad transformer.
-}
newtype MapperT b r m x = SM (StateT (MapperState b r) m x)
    deriving (Functor, Applicative, Monad, MonadTrans)

-- | Non-transformer variant of `MapperT`
type Mapper b r = MapperT b r Identity

-- | A `MapperT` that never yields any values
type ReducerT r m x = forall b. MapperT b r m x

-- | A `Mapper` that never yields any values
type Reducer r x = forall b. Mapper b r x

-- | A `MapperT` that doesn't reduce anything
type BuilderT b m x = forall r. MapperT b r m x

-- | A `Mapper` that doesn't reduce anything
type Builder b x = forall r. Mapper b r x

-- | A `Mapper` that produces a span via efficient state thread
type MapperS b r x = forall s. MapperT (MutSpan s b) r (ST s) x

type BuilderS b x = forall s r. MapperT (MutSpan s b) r (ST s) x

-- | A `Mapper` that builds within the `ST` monad
type MapperST b r x = forall s. MapperT (b s) r (ST s) x

-- * Interface

-- | Appends a single value onto the result buffer
yield :: (Snoc b b x x, Monad m) => x -> MapperT b r m ()
yield = yield' id

-- | Variant of `yield` inside a lens
yield' :: (Snoc b' b' x x, Monad m) => Lens' b b' -> x -> MapperT b r m ()
yield' l b = SM $ modify $ over (output . l) (`snoc` b)

-- | Variant of `yield` for producing a `Span`
yieldS :: x -> MapperS x r ()
yieldS x = yieldS' id x

-- | Variant of `yieldS` inside a lens
yieldS' :: Lens' b (MutSpan s x) -> x -> MapperT b r (ST s) ()
yieldS' l x = SM $ modifyM $ output $ l \b -> snocMutSpan b x

{- | Gets the singleton span immediately to the left of the next input.
  If no input has been consumed, returns a 0-length span.
-}
getLastSpan :: (Monad m, ISpan r) => MapperT b r m r
getLastSpan =
    SM $
        gets rest <§ \r ->
            let b = baseSpan r
             in case r `isSliceOf` b of
                    Nothing -> error "getLeftPos: ISpan instance violates baseSpan law"
                    Just 0 -> slice 0 0 r
                    Just n -> slice (n - 1) 1 b

-- | Equivalent to `fst $> trace pop` without advancing state
peekSpan :: (Monad m, ISpan r) => MapperT b r m r
peekSpan = SM $ gets rest <§ \r -> takes (signum $ size r) r

-- | Inspects the span consumed by another mapping
trace :: (Monad m, ISpan r) => MapperT b r m x -> MapperT b r m (r, x)
trace (SM f) = SM do
    prev <- gets rest
    x <- f
    post <- gets rest
    return case post `isSliceOf` prev of
        Nothing -> error "trace: ISpan instance violates uncons law"
        Just o -> (takes o prev, x)

-- | Variant of `trace` that completes a function instead of building a tuple
trace' :: (Monad m, ISpan r) => MapperT b r m (r -> x) -> MapperT b r m x
trace' x = uncurry (flip ($)) $> trace x

-- | Previews the next return value of `pop`
peek :: (Monad m, Uncons r a) => MapperT b r m (Maybe a)
peek = SM $ gets $ fmap fst . uncons . rest

-- | Consumes a single input token
pop :: (Monad m, Uncons r a) => MapperT b r m (Maybe a)
pop = SM $ state \s -> case uncons (rest s) of
    Just (a, r) -> (Just a, s{rest = r})
    Nothing -> (Nothing, s)

-- | Inspects the next token, then decides if it should be consumed and which mapper to continue with
popWhen :: (Monad m, Uncons r x) => (Maybe x -> (Bool, MapperT b r m y)) -> MapperT b r m y
popWhen f = join $ SM $ state \s -> case uncons (rest s) of
    Just (a, r) -> let (y, z) = f (Just a) in (z, if y then s{rest = r} else s)
    Nothing -> (snd $ f Nothing, s)

-- | Executes a mapping on an input span in some monad
runMapperT :: (AsEmpty b, Monad m) => MapperT b r m x -> r -> m (b, x, r)
runMapperT (SM f) r = runStateT f (S r Empty) <§ \(x, S r' b') -> (b', x, r')

-- | Executes a mapping on an input span
runMapper :: (AsEmpty b) => MapperT b r Identity x -> r -> (b, x, r)
runMapper = runIdentity .: runMapperT

-- | Executes a mapping that produces a `Span`
runMapperS :: (Prim b) => MapperS b r x -> r -> (Span b, x, r)
runMapperS m r0 = runST $ runMapperST newMutSpan unsafeFreezeSpan m r0

-- | Executes a mapping inside the ST monad with lifted computations for the build result
runMapperST :: ST s b -> (b -> ST s b') -> MapperT b r (ST s) x -> r -> ST s (b', x, r)
runMapperST init fin (SM f) r0 = do
    b0 <- init
    (x, S r b) <- runStateT f (S r0 b0)
    b' <- fin b
    return (b', x, r)

-- | Executes a builder without performing a reduction
runBuilderT :: (AsEmpty b, Monad m) => BuilderT b m x -> m (b, x)
runBuilderT b = runMapperT b () <§ \(a, b, ()) -> (a, b)

-- | Executes a builder without performing a reduction
runBuilder :: (AsEmpty b) => Builder b x -> (b, x)
runBuilder b = runIdentity $ runBuilderT b

-- | Executes a builder that produces a `Span`
runBuilderS :: (Prim b) => BuilderS b x -> (Span b, x)
runBuilderS b = let (x, y, ()) = runMapperS b () in (x, y)

-- | Executes a reducer without building
runReducerT :: (Functor m) => ReducerT r m x -> r -> m (x, r)
runReducerT (SM f) r = runStateT f (S r ()) <§ \(x, S r' ()) -> (x, r')

-- | Executes a reducer without building
runReducer :: Reducer r x -> r -> (x, r)
runReducer r = runIdentity . runReducerT r
