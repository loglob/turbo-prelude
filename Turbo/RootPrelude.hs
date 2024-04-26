{-# OPTIONS_HADDOCK hide #-}

-- | The prelude sans anything defined in this package
module Turbo.RootPrelude (module Exp, drop, replicate, splitAt, take, maximum1, minimum1, maximumBy1, minimumBy1) where

import Control.Applicative as Exp (Alternative (..), Applicative (..), liftA, liftA3)
import Control.Lens as Exp (
    At (..),
    Cons (..),
    Field1 (..),
    Field10 (..),
    Field11 (..),
    Field12 (..),
    Field13 (..),
    Field14 (..),
    Field15 (..),
    Field16 (..),
    Field17 (..),
    Field18 (..),
    Field19 (..),
    Field2 (..),
    Field3 (..),
    Field4 (..),
    Field5 (..),
    Field6 (..),
    Field7 (..),
    Field8 (..),
    Field9 (..),
    Getter,
    Getting,
    Index,
    IxValue,
    Ixed (..),
    Lens,
    Lens',
    Setter,
    Setting,
    Setting',
    makeLenses,
    over,
    set,
    view,
    _head,
    _tail,
 )
import Control.Monad as Exp (
    Monad (..),
    MonadFail (..),
    MonadPlus (..),
    ap,
    filterM,
    foldM,
    foldM_,
    forM,
    forever,
    guard,
    join,
    liftM,
    liftM2,
    liftM3,
    liftM4,
    liftM5,
    mapAndUnzipM,
    mfilter,
    replicateM,
    replicateM_,
    sequence_,
    unless,
    void,
    when,
    zipWithM,
    zipWithM_,
    (<=<),
    (=<<),
    (>=>),
 )
import Data.Bifoldable as Exp (Bifoldable (..))
import Data.Bifunctor as Exp (Bifunctor (..))
import Data.Bitraversable as Exp (Bitraversable (..), bimapM)
import Data.Bool as Exp (Bool (..), bool, not, otherwise, (&&), (||))
import Data.Char as Exp (Char, chr, ord)
import Data.Either as Exp (Either (..), either, lefts, partitionEithers, rights)
import Data.Eq as Exp (Eq (..))
import Data.Foldable as Exp (
    Foldable (
        elem,
        fold,
        foldMap,
        foldMap',
        foldl,
        foldl',
        foldr,
        foldr',
        length,
        null,
        product,
        sum,
        toList
    ),
    all,
    any,
    asum,
    concat,
    concatMap,
    find,
    foldlM,
    foldrM,
    forM_,
    for_,
    mapM_,
    msum,
    notElem,
    sequenceA_,
    traverse_,
 )
import Data.Foldable1 as Exp (
    Foldable1 (
        fold1,
        foldMap1,
        foldMap1',
        foldlMap1,
        foldlMap1',
        foldrMap1,
        foldrMap1',
        toNonEmpty
    ),
    foldl1,
    foldl1',
    foldlM1,
    foldlMapM1,
    foldr1,
    foldr1',
    foldrM1,
    foldrMapM1,
 )
import Data.Foldable1 qualified as F1
import Data.Function as Exp (const, flip, id, ($), (.))
import Data.Functor as Exp (Functor (fmap), unzip)
import Data.Functor.Const as Exp (Const (..))
import Data.Functor.Identity as Exp (Identity (..))
import Data.List as Exp (
    break,
    cycle,
    delete,
    deleteBy,
    deleteFirstsBy,
    dropWhile,
    dropWhileEnd,
    elemIndex,
    elemIndices,
    filter,
    findIndex,
    findIndices,
    group,
    groupBy,
    inits,
    insert,
    insertBy,
    intercalate,
    intersect,
    intersectBy,
    intersperse,
    isInfixOf,
    isPrefixOf,
    isSubsequenceOf,
    isSuffixOf,
    iterate,
    iterate',
    map,
    nub,
    nubBy,
    partition,
    permutations,
    repeat,
    reverse,
    scanl,
    scanl',
    scanr,
    sort,
    sortBy,
    sortOn,
    span,
    stripPrefix,
    subsequences,
    tails,
    takeWhile,
    unfoldr,
    unsnoc,
    zip,
    zipWith,
    zipWith3,
    zipWith4,
    zipWith5,
    zipWith6,
    zipWith7,
    (!?),
    (++),
 )
import Data.List qualified as L
import Data.List.NonEmpty as Exp (
    NonEmpty (..),
    head,
    init,
    inits1,
    last,
    nonEmpty,
    prependList,
    scanl1,
    scanr1,
    some1,
    tail,
    tails1,
    (<|),
 )
import Data.Map as Exp (Map)
import Data.Maybe as Exp (
    Maybe (..),
    catMaybes,
    fromJust,
    fromMaybe,
    isJust,
    isNothing,
    listToMaybe,
    mapMaybe,
    maybe,
    maybeToList,
 )
import Data.Monoid as Exp (Monoid (..))
import Data.Ord as Exp (Ord (..), Ordering (..), clamp, comparing)
import Data.Semigroup as Exp (Semigroup (..))
import Data.Set as Exp (Set)
import Data.String as Exp (IsString (..), String, lines, unlines, unwords, words)
import Data.Text as Exp (Text, pack, unpack)
import Data.Traversable as Exp (
    Traversable (..),
    fmapDefault,
    foldMapDefault,
    for,
    forAccumM,
    mapAccumL,
    mapAccumM,
    mapAccumR,
 )
import Data.Tuple as Exp (Solo (Solo), curry, fst, getSolo, snd, swap, uncurry)
import GHC.Base as Exp (($!))
import GHC.Enum as Exp (Bounded (..), Enum (..))
import GHC.Float as Exp (
    Double,
    Float,
    Floating (..),
    RealFloat (..),
    floatToDigits,
    fromRat,
    isDoubleDenormalized,
    isDoubleFinite,
    isDoubleInfinite,
    isDoubleNaN,
    isDoubleNegativeZero,
    isFloatDenormalized,
    isFloatFinite,
    isFloatInfinite,
    isFloatNaN,
    isFloatNegativeZero,
 )
import GHC.Int as Exp (Int (..), Int16 (..), Int32 (..), Int64 (..), Int8 (..))
import GHC.Num as Exp (Integer, Natural (..), Num (..))
import GHC.Real as Exp (
    Fractional (..),
    Integral (..),
    Ratio (..),
    Rational,
    Real (..),
    RealFrac (..),
    denominator,
    even,
    fromIntegral,
    gcd,
    infinity,
    lcm,
    notANumber,
    numerator,
    odd,
    realToFrac,
    (%),
    (^),
    (^^),
 )
import GHC.Show as Exp (
    Show (..),
    ShowS,
    showList__,
    showLitChar,
    showLitString,
    showParen,
    shows,
 )
import GHC.Word as Exp (Word (..), Word16 (..), Word32 (..), Word64 (..), Word8 (..))
import System.IO as Exp (
    BufferMode (..),
    FilePath,
    Handle,
    HandlePosn,
    IO,
    IOMode (..),
    Newline (..),
    NewlineMode (..),
    SeekMode (..),
    TextEncoding,
    appendFile,
    hClose,
    hFileSize,
    hFlush,
    hGetBuf,
    hGetBufNonBlocking,
    hGetBufSome,
    hGetBuffering,
    hGetChar,
    hGetContents,
    hGetContents',
    hGetEcho,
    hGetEncoding,
    hGetLine,
    hGetPosn,
    hIsClosed,
    hIsEOF,
    hIsOpen,
    hIsReadable,
    hIsSeekable,
    hIsTerminalDevice,
    hIsWritable,
    hLookAhead,
    hPrint,
    hPutBuf,
    hPutBufNonBlocking,
    hPutChar,
    hPutStr,
    hPutStrLn,
    hReady,
    hSeek,
    hSetBinaryMode,
    hSetBuffering,
    hSetEcho,
    hSetEncoding,
    hSetFileSize,
    hSetNewlineMode,
    hSetPosn,
    hShow,
    hTell,
    hWaitForInput,
    interact,
    localeEncoding,
    openBinaryFile,
    openBinaryTempFile,
    openBinaryTempFileWithDefaultPermissions,
    openFile,
    openTempFile,
    openTempFileWithDefaultPermissions,
    readFile,
    readFile',
    readIO,
    stderr,
    stdin,
    stdout,
    withBinaryFile,
    withFile,
    writeFile,
 )
import Text.Printf as Exp (hPrintf, printf)

drop :: (Integral i) => i -> [a] -> [a]
drop = L.genericDrop

replicate :: (Integral i) => i -> a -> [a]
replicate = L.genericReplicate

splitAt :: (Integral i) => i -> [a] -> ([a], [a])
splitAt = L.genericSplitAt

take :: (Integral i) => i -> [a] -> [a]
take = L.genericTake

maximum1 :: (Foldable1 f, Ord a) => f a -> a
maximum1 = F1.maximum

minimum1 :: (Foldable1 f, Ord a) => f a -> a
minimum1 = F1.minimum

maximumBy1 :: (Foldable1 f) => (a -> a -> Ordering) -> f a -> a
maximumBy1 = F1.maximumBy

minimumBy1 :: (Foldable1 f) => (a -> a -> Ordering) -> f a -> a
minimumBy1 = F1.minimumBy
