module Turbo.CastTH (makeInstances) where
import Language.Haskell.TH
import GHC.Err (undefined, error)
import Turbo.RootPrelude

data Width = W8 | W16 | W32 | W | W64 deriving (Eq, Show, Enum, Bounded, Ord)

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

convert :: T -> T -> Exp -> Exp
convert from to
 | from == to = undefined
-- unbox everything
 | boxed from = error "Input should be unboxed earlier!"
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

mkInst :: T -> T -> Dec
mkInst long short = i
 where
    i = InstanceD Nothing [] (cls `AppT` t long `AppT` t short) [
        method "extend" $ unbox short $ convert short{ boxed=False } long ,
        method "narrow" $ unbox long  $ convert long{ boxed=False } short
     ]

    method :: String -> Clause -> Dec
    method n c = FunD (mkName n) [ c ]

    unbox :: T -> (Exp -> Exp) -> Clause
    unbox i b = Clause [pat i] (NormalB $ b $ VarE arg) []

    arg = mkName "x"
    -- boxing-agnostic argument pattern
    pat :: T -> Pat
    pat t = if boxed t then ConP (boxCtor t) [] [VarP arg] else VarP arg
    cls = ConT $ mkName ":>:"

followPairs :: [a] -> [(a,a)]
followPairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]

makeInstances :: Q [Dec]
makeInstances = return do
    (short,long) <- followPairs [minBound .. maxBound]
    sb <- [ True, False ]
    lb <- [ True, False ]
    -- widening a short unsigned into a long signed is valid
    (si,li) <- [ (True, True), (False, False), (False,True) ]
    let s = T short sb si
    let l = T long lb li

    return (mkInst l s)
