module Turbo.Cast.TH (makeExtendInstances, makeIsSignedInstances) where
import Language.Haskell.TH
import GHC.Err (undefined, error)
import Turbo.RootPrelude
import Turbo.Extra (both)
import GHC.Exts ((<#))
import Data.Bits (Bits(shiftL, shiftR))

data Width = W8 | W16 | W32 | W | W64 deriving (Eq, Show, Enum, Bounded, Ord)

bits :: Width -> Int
bits W8  = 8
bits W16 = 16
bits W32 = 32
bits W   = undefined
bits W64 = 64

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

t :: T -> Type
t x = ConT $ mkName $ show x

directCast :: T -> T -> Exp
directCast (T fW _ fS) (T tW _ tS) = VarE $ mkName $ f where
    f
     | fW == W && tW == W = lt ++ "2" ++ rt ++ "#"
     | otherwise          = lt ++ suffix fW ++ "To" ++ rt ++ suffix tW ++ "#"
    lt = (if fS then "int" else "word")
    rt = (if tS then "Int" else "Word")

boxCtor :: T -> Name
boxCtor (T w _ s) = mkName $ (if s then "I" else "W") ++ suffix w ++ "#"

boxPat :: T -> Name -> Pat
boxPat t name = if boxed t then BangP (ConP (boxCtor t) [] [VarP name]) else VarP name

convert :: T -> T -> Exp -> Exp
convert from to
 | from == to = id
-- unbox everything
 | boxed from = \x -> let
        tmp = mkName "y"
        from' = from{ boxed = False }
    in 
        LetE [ ValD (boxPat from tmp) (NormalB x) [] ] (convert from' to $ VarE tmp)
 | boxed to   = AppE (ConE (boxCtor to)) . convert from to{ boxed = False }
-- fix signedness
 | signed from /= signed to = case()of 
   _| width from == width to -> AppE (directCast from to)
    -- ensure no sign extension happens by changing width within unsigned types
    | signed from            -> let from' = from{ signed = False } in convert from' to . AppE (directCast from from')
    | otherwise              -> let to' = to{ signed = False } in AppE (directCast to' to) . convert from to'
-- actually convert widths
 | width from == W || width to == W = AppE (directCast from to)
 | otherwise = convert from{ width=W } to . convert from to{ width = W }

-- primitiveCast from to = (VarE $ mkName $ "int" ++ from ++ "ToInt" ++ to ++ "#")
-- primitiveCast from to = (primitiveCast from "") `comp` (primitiveCast "" to)

func :: String -> (Name -> Pat) -> (Exp -> Exp) -> Dec
func name pat body = FunD (mkName name) [
    Clause [pat tmp] (NormalB $ body $ VarE tmp) []
 ] where
    tmp = mkName "x"

mkExtend :: T -> T -> Dec
mkExtend long short = i
 where
    i = InstanceD Nothing [] (cls `AppT` t long `AppT` t short) [
        func "extend" (boxPat short) (convert short{ boxed=False } long),
        func "narrow" (boxPat long)  (convert long{ boxed=False }  short)
     ]
    cls = ConT $ mkName ":>:"

followPairs :: [a] -> [(a,a)]
followPairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]

makeExtendInstances :: Q [Dec]
makeExtendInstances = return do
    (short,long) <- followPairs widths
    sb <- [ True, False ]
    lb <- [ True, False ]
    -- widening a short unsigned into a long signed is valid
    (si,li) <- [ (True, True), (False, False), (False,True) ]
    let s = T short sb si
    let l = T long lb li
    return (mkExtend l s)

constant :: T -> Integer -> Exp
constant t x
 | boxed t   = LitE $ IntegerL x -- these are instances of `Number`
 | otherwise = convert t{width = W} t $ LitE $ (if signed t then IntPrimL else WordPrimL) x -- TH cannot generate e.g. `0#Int32` ?

-- | Generates an expression that evaluates to True if $2 (of type $1) is negative and False otherwise
isNegative :: T -> Exp -> Exp
isNegative t n
 | boxed t   = InfixE (Just n) (VarE '(<)) (Just zero)
-- isTrue# (i `ltInt16#` 0#Int16)
 | otherwise = (VarE 'isTrue#) `AppE` (VarE lt `AppE` n `AppE` zero)
 where
    lt | width t == W = '(<#)
       | otherwise    = mkName$ "lt" ++ show t
    zero = constant t 0

-- | Generates an expression that evaluates to the maximum value of a type
maxSafeValue :: T -> Exp
maxSafeValue t 
 | boxed t      = VarE 'maxBound -- are always bounded
 | width t == W = let t' = t{ boxed=True } in convert t' t $ maxSafeValue t' -- forward lifted definition
 -- these have fixed bit size
 | otherwise    = let
    b = bits (width t)
    tooLong = (1 :: Integer) `shiftL` b
    fullSet = tooLong - 1
    woSign = fullSet `shiftR` 1
 in 
    constant t woSign

makeIsSignedInstances :: Q [Dec]
makeIsSignedInstances = return do
    w <- widths
    boxed <- [ True, False ]
    let (signed, unsigned) = both (T w boxed) (True, False)
    -- | name has to be late-resolved
    let sig = ConT (mkName "IsSigned") `AppT` t signed `AppT` t unsigned
    -- | Makes an unsign* function, that inserts $2 when the input was negative
    let mkUnsign x fallback = CondE (isNegative signed x) fallback (convert signed unsigned x)
    let tmp = mkName "y"
    -- | Makes a sign* function, that inserts $2 when the conversion overflows
    --   Testing sign after conversion is sufficient since the bit-width doesn't change
    let mkSign x fallback = LetE [
            ValD (VarP tmp) (NormalB$ convert unsigned signed x) []
         ]$ CondE (isNegative signed$ VarE tmp) fallback (VarE tmp)

    let mkError msg = (VarE 'error) `AppE` LitE (StringL msg)
    
    return$ InstanceD Nothing [] sig [
        func "sign" VarP \x -> mkSign x (mkError "argument of sign is negative"),
        func "signSat" VarP \x -> mkSign x (maxSafeValue signed),
        func "unsign" VarP \x -> mkUnsign x (mkError "argument of unsign is out of bounds"),
        func "unsignSat" VarP \x -> mkUnsign x (constant unsigned 0)
     ]
