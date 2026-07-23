module Turbo.CastTH (makeInstances) where
import Language.Haskell.TH
import GHC.Err (undefined)
import Turbo.RootPrelude

data Width = W8 | W16 | W32 | W | W64 deriving (Eq, Show, Enum, Bounded, Ord)
data T = Boxed Width | Unboxed Width deriving (Eq, Show)

suffix :: Width -> String
suffix x = case show x of
    'W' : r -> r
    _ -> undefined

suffixT :: T -> String
suffixT (Boxed x)   = suffix x
suffixT (Unboxed x) = suffix x ++ "#"

t :: T -> Type
t x = ConT $ mkName $ "Int" ++ suffixT x

width :: T -> Width
width (Unboxed w) = w
width (Boxed w) = w

primitiveCast :: Width -> Width -> Exp -> Exp
primitiveCast from to x = y where
    y = if from == to then x 
      else if from == W || to == W then direct `AppE` x
      else primitiveCast W to $ primitiveCast from W x
    direct = VarE $ mkName $ "int" ++ suffix from ++ "ToInt" ++ suffix to ++ "#"

boxCtor :: Width -> Name
boxCtor w = mkName ("I" ++ suffix w ++ "#")

-- primitiveCast from to = (VarE $ mkName $ "int" ++ from ++ "ToInt" ++ to ++ "#")
-- primitiveCast from to = (primitiveCast from "") `comp` (primitiveCast "" to)

mkInst :: T -> T -> Dec
mkInst long short = i
 where
    i = InstanceD Nothing [] (cls `AppT` t long `AppT` t short) [
        method "extend" $ unbox short \x -> rebox long  $ primitiveCast (width short) (width long) x,
        method "narrow" $ unbox long  \x -> rebox short $ primitiveCast (width long)  (width short) x
     ]

    method :: String -> Clause -> Dec
    method n c = FunD (mkName n) [ c ]

    unbox :: T -> (Exp -> Exp) -> Clause
    unbox i b = Clause [pat i] (NormalB $ b $ VarE arg) []

    rebox :: T -> Exp -> Exp
    rebox (Unboxed _) x = x
    rebox (Boxed w)   x = ConE (boxCtor w) `AppE` x 

    arg = mkName "x"
    -- boxing-agnostic argument pattern
    pat :: T -> Pat
    pat (Unboxed _) = VarP arg
    pat (Boxed w) = ConP (boxCtor w) [] [VarP arg]
    cls = ConT $ mkName ":>:"

followPairs :: [a] -> [(a,a)]
followPairs xs = [ (x,y) | (x:rest) <- tails xs, y <- rest ]

makeInstances :: Q [Dec]
makeInstances = return do
    (short,long) <- followPairs [minBound .. maxBound]
    let fs = [ Unboxed, Boxed ]
    s <- [ f short | f <- fs ]
    l <- [ f long | f <- fs ]
    return $ mkInst l s
