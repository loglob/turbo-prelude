module Turbo.Cast.TH (makeExtend, makeIsSigned, makeBoxedIntegers, deriveBoxed) where
import GHC.Err (undefined, error)
import GHC.Exts
import Language.Haskell.TH
import Turbo.Cast.Classes
import Turbo.Extra (both)
import Turbo.Operators
import Turbo.RootPrelude
import Data.Char (toLower)
import Data.Foldable (foldl)

data Width = W8 | W16 | W32 | W | W64 deriving (Eq, Show, Enum, Bounded, Ord)

widths :: [Width]
widths = [minBound .. maxBound] :: [Width]

suffix :: Width -> String
suffix x = case show x of
    'W' : r -> r
    _ -> undefined

data T = T {
    width :: Width,
    boxed :: Bool ,
    signed :: Bool
 } deriving (Eq)

instance Show T where
  show t = bt ++ (suffix $ width t) ++ hash
    where
        bt = if signed t then "Int" else "Word"
        hash = if boxed t then "" else "#"

t :: T -> Q Type
t x = conT $ mkName $ show x

ne :: Eq b => (a -> b) -> a -> a -> Bool
ne f x y = f x /= f y

-- | finds the primitive casting function between two types
directCast :: T -> T -> Q Exp
directCast from to
 | from == to             = error "directCast used with same type twice"
 | boxed from || boxed to = error "directCast used with boxed types"
 | ne signed from to 
     && ne width from to  = error$ "No direct conversion exists between " ++ show from ++ " and " ++ show to 
 | otherwise = let
        left   = toLower $> show from{boxed=True} -- skips trailing #
        middle = if both width (from,to) == (W,W) then "2" else "To"
        right  = show to
    in 
        varE$ mkName$ left++middle++right

-- | Casts between integer types
cast :: T -> T -> Q Exp -> Q Exp
cast from to exp
 | from == to = exp
 -- lower to unboxed parameters
 | boxed to   = [e| box $(cast from to{boxed=False} exp) |]
 | boxed from = cast from{boxed=False} to [e| unbox $exp |]
 -- fix signedness
 | signed from /= signed to = 
    if width from == width to then [e| $(directCast from to) $exp |]
    -- ensure no sign extension happens
    else if signed from then let tmp = from{signed=False} in cast tmp to [e| $(directCast from tmp) $exp |] 
    else let tmp = to{signed=False} in [e| $(directCast tmp to) $(cast from tmp exp) |]
 -- convert widths
 | width from /= W && width to /= W = let tmp = from{ width=W } in cast tmp to (cast from tmp exp)
 | otherwise                        = [e| $(directCast from to) $exp |]

followPairs :: [a] -> [(a,a)]
followPairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]

makeExtend :: Q [Dec]
makeExtend = join $> sequence do
    (sw,lw) <- followPairs widths
    sb <- [ True, False ]
    lb <- [ True, False ]
    -- widening a short unsigned into a long signed is valid
    (si,li) <- [ (True, True), (False, False), (False,True) ]
    let short = T sw sb si
    let long = T lw lb li
    return [d|
        instance $(t long) :>: $(t short) where
            extend x = $(cast short long [e| x |])
            narrow x = $(cast long short [e| x |])
     |]
    
constant :: T -> Integer -> Q Exp
constant t x
 | boxed t      = litE $ IntegerL x -- these are instances of `Number`
 | width t == W = litE $ (if signed t then IntPrimL else WordPrimL) x
 -- TH cannot generate e.g. `0#Int32` ?
 | otherwise    = let t' = t{width=W} in cast t' t (constant t' x)

-- | Generates an expression that evaluates to True if $2 (of type $1) is negative and False otherwise
isNegative :: T -> Q Exp -> Q Exp
isNegative t n = case t of
    T _ True _ -> [e| $n < 0 |]
    T W _ True -> [e| isTrue# ($n <# 0#) |]
    _          -> let lt = varE$ mkName ("lt" ++ show t)
                  in [e| isTrue# ($lt $n $(constant t 0)) |]

-- | Generates an expression that evaluates to the maximum value of a type
maxSafeValue :: T -> Q Exp
maxSafeValue t = if boxed t then [e| maxBound |] else [e| unbox maxBound |]

makeIsSigned :: Q [Dec]
makeIsSigned = join $> sequence do
    w <- widths
    boxed <- [ True, False ]
    let (signed, unsigned) = both (T w boxed) (True, False)

    let isNeg = isNegative signed

    -- | Negates an unsigned value after bitcasting from signed, with correct behavior for the minimum signed value 
    let totalNegate x = if boxed then [e| (complement $x) + 1 |] else case w of
            W -> [e| plusWord# (not# $x) 1## |]
            W64 -> [e| plusWord64# (not64# $x) $(constant unsigned 1) |]
            _ -> let
                not = varE$ mkName$ "notWord"++ suffix w++"#"
                plus = varE$ mkName$ "plusWord"++suffix w++"#"
             in
                [e| $plus ($not $x) $(constant unsigned 1) |]

    let mkSign   x fallback = [e| let y = $(cast unsigned signed x) in if $(isNeg [e| y |]) then $fallback else y |]
    let mkUnsign x fallback = [e| if $(isNeg x) then $fallback else $(cast signed unsigned x) |]

    return [d|
        instance IsSigned $(t signed) $(t unsigned) where
            sign    x = $(mkSign [e| x |] [e| error "input to sign was negative" |])
            signSat x = $(mkSign [e| x |] (maxSafeValue signed))
            unsign    x = $(mkUnsign [e| x |] [e| error "input to unsign was negative" |])
            unsignSat x = $(mkUnsign [e| x |] (constant unsigned 0))
            abs#      x = let y = $(cast signed unsigned [e| x |]) in if $(isNeg [e| x |]) then $(totalNegate [e| y |]) else y
     |]

makeBoxedIntegers :: Q [Dec]
makeBoxedIntegers = join $> sequence do
    w <- widths
    s <- [True,False]
    let ctor = mkName$ (if s then "I" else "W") ++ suffix w ++ "#"

    return [d|
        instance Boxed $(t T{width=w, boxed=False, signed=s}) $(t T{width=w, boxed=True, signed=s}) where
            unbox $(conP ctor [[p| x |]]) = x
            box x = $(conE ctor) x
     |]

deriveBoxed :: Name -> Q [Dec]
deriveBoxed name = do
    info <- reify name 
    tc <- case info of
        TyConI x -> return x
        _ -> fail $ show name ++ " must refer to a type, but is " ++ show info
    (tv,ctor) <- case tc of
        DataD _ _ x _ [y] _ -> return (x,y)
        DataD _ _ _ _ _   _ -> fail "Boxed type must have exactly one constructor"
        _ -> fail "Boxed type must be defined as data"
    (c,primType) <- case ctor of
        NormalC   c    [(_,t)] -> return (c,t)
        RecC      c  [(_,_,t)] -> return (c,t)
        GadtC    [c]   [(_,t)] _ -> return (c,t)
        RecGadtC [c] [(_,_,t)] _ -> return (c,t)
        -- InfixC, ForallC
        _ -> fail$ "Constructor for " ++ show name ++ " must accept a single argument"
    let boxedType = foldl (AppT .° VarT) (ConT name) (tv <§ \case
            PlainTV  n _   -> n
            KindedTV n _ _ -> n)
    let tmp = mkName "x" -- needed because names cannot be spliced
    [d|
        instance Boxed $(pure primType) $(pure boxedType) where
            box x = $(conE c) x
            unbox $(conP c [varP tmp]) = $(varE tmp)
     |]
