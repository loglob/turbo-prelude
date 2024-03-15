module Control.Attempt where
import Turbo.Prelude

-- | Represents an attempt that may fail silently or with signal.
--   
--   Note that its `Monad` instance does not fulfill `a >> b  =  a >>= const b`.
--   For example `SilentNo >> SignalNo = SignalNo` but `SilentNo >>= const SignalNo = SilentNo`.
--
--  (!) Its `MonadPlus` instance doesn't follow the MonadPlus laws exactly.
--  Because `mzero` is a silent failure, which may be promoted to signalling failure, it isn't a right zero for `>>`.
data Attempt x =
    -- | Fails with signal. Acts as an absorbing element in <*> and absorbs SilentNo in <|>
    SignalNo |
    -- | Fails silently. Overwritten by SignalNo in <*> and by Success in <|>
    SilentNo |
    -- | A successful computation. Overwrites failure in <|> and is overwritten by failure in <*>. 
    Success x
  deriving (Eq, Ord, Show)

instance Functor Attempt where
    fmap _  SignalNo   = SignalNo
    fmap _  SilentNo   = SilentNo
    fmap f (Success x) = Success (f x)
    
instance Applicative Attempt where
    pure = Success
    liftA2 f (Success x) (Success y) = Success (f x y)
    liftA2 _ (SignalNo ) (_        ) = SignalNo
    liftA2 _ (_        ) (SignalNo ) = SignalNo
    liftA2 _ (SilentNo ) (Success _) = SilentNo
    liftA2 _ (Success _) (SilentNo ) = SilentNo
    liftA2 _ (SilentNo ) (SilentNo ) = SilentNo

instance Monad Attempt where
    Success x >>= f = f x
    SilentNo  >>= _ = SilentNo
    SignalNo  >>= _ = SignalNo
    (>>) = (*>)

instance Alternative Attempt where
    empty = SilentNo
    Success x <|> _         = Success x
    _         <|> Success x = Success x
    SignalNo  <|> _         = SignalNo
    _         <|> SignalNo  = SignalNo
    SilentNo  <|> SilentNo  = SilentNo

instance MonadPlus Attempt


-- * Introduction
-- | fails silently if a maybe isn't present
silent :: Maybe x -> Attempt x
silent = maybe SilentNo Success

-- | fails with signal if a maybe isn't present
loud :: Maybe x -> Attempt x
loud = maybe SignalNo Success


-- * Elimination
-- | Determines if an attempt failed with signal
signals :: Attempt x -> Bool
signals  SignalNo   = True
signals  SilentNo   = False
signals (Success _) = False

-- | De-constructs `Success` case
success :: Attempt x -> Maybe x
success SignalNo    = Nothing
success SilentNo    = Nothing
success (Success x) = Just x

-- * Monad Functions
-- | Runs a computation until a signalling failure occurs
untilSig :: Monad m => m (Attempt x) -> m [x]
untilSig p = u where
    u = p >>= \case
        SignalNo  -> return []
        SilentNo  -> u
        Success x -> x >:> u

-- | Repeats a computation until it either succeeds or fails with signal
keepTrying :: Monad m => m (Attempt x) -> m (Maybe x)
keepTrying p = y where
    y = p >>= \case
        SignalNo -> return Nothing
        SilentNo -> y
        Success x -> return (Just x)
