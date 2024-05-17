module Turbo.ExtraTH where

import Language.Haskell.TH
import Turbo.RootPrelude

infixr 0 →

-- | Shorthand for constructing function types
(→) :: Type -> Type -> Type
a → b = AppT (AppT ArrowT a) b

tup :: [Type] -> Type
tup ts = foldl AppT (TupleT $ length ts) ts

fun :: [Type] -> Type -> Type
fun ts t = foldr (→) t ts

tv :: Name -> TyVarBndr Specificity
tv a = PlainTV a SpecifiedSpec

mkTupleMap :: Int -> Q [Dec]
mkTupleMap n = do
    let nm = mkName ("map" ++ show n)
    let f = mkName "f"
    xs <- mapM newName $ replicate n "x"
    let a = VarT $ mkName "a"
    let b = VarT $ mkName "b"
    let
    return
        [ SigD nm ((a → b) → tup (replicate n a) → tup (replicate n b)),
          FunD
            nm
            [ Clause
                [VarP f, TupP (fmap VarP xs)]
                (NormalB $ TupE $ map (Just . AppE (VarE f) . VarE) xs)
                []
            ]
        ]

mkTupleMaps :: [Int] -> Q [Dec]
mkTupleMaps ns = concat `fmap` mapM mkTupleMap ns

mkTupleColl :: Int -> Q [Dec]
mkTupleColl n = do
    let nm = mkName ("list" ++ show n)
    xs <- mapM newName $ replicate n "x"
    let a = VarT $ mkName "a"
    let tup t = foldl AppT (TupleT n) (replicate n t)
    return
        [ SigD nm (tup a → (ListT `AppT` a)),
          FunD
            nm
            [ Clause [TupP (fmap VarP xs)] (NormalB $ ListE $ VarE `fmap` xs) []
            ]
        ]

mkTupleColls :: [Int] -> Q [Dec]
mkTupleColls ns = concat `fmap` mapM mkTupleColl ns
