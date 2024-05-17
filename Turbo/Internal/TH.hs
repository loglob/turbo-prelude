module Turbo.Internal.TH where

import Turbo.ExtraTH

import Language.Haskell.TH
import Turbo.Internal.Classes
import Turbo.Operators
import Turbo.RootPrelude

_if :: Bool -> [a] -> [a]
_if True x = x
_if False _ = []

-- | Makes `map*` functions from `maps*` functions in `MapMaybe`
mkMap :: [Name] -> Q [Dec]
mkMap ns = fmap concat $ forM ns \n -> case nameBase n of
    'm' : 'a' : 'p' : 's' : xs ->
        let
            n' = mkName ("map" ++ xs)
            f = mkName "f"
            x = mkName "x"
         in
            return
                [ PragmaD (InlineP n' Inline FunLike AllPhases),
                  FunD
                    n'
                    [Clause [VarP f, VarP x] (NormalB (VarE n `AppE` VarE f `AppE` VarE x `AppE` ConE 'Empty)) []]
                ]
    _ -> fail $ "Name should start with \"maps\": " ++ nameBase n

{- | Generates map* variants
    $1: with Index
    $2: with Maybe
    $3: with Monad
    $4: in situ
    $5: Uncons / Unsnoc
    $6: Cons / Snoc
-}
mkOneMap :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Q [Dec]
mkOneMap i o m s r b =
    let
        regular = (r, b) /= (False, True)
        lr = (if r then "L" else "R") ++ _if (b /= r) (if b then "L" else "R")
        n = mkName $ "map" ++ _if s "s" ++ _if o "Maybe" ++ _if i "Ix" ++ lr ++ _if m "M"
        base = mkName $ "mapsMaybe" ++ (_if regular "Ix") ++ lr ++ "M"
        f = mkName "f"
        xs = mkName "xs"
        ys = mkName "ys"
        x = mkName "x"
        j = mkName "i"
        f' =
            LamE
                (_if regular [if i then VarP j else SigP WildP (ConT ''Int)] ++ [VarP x])
                ( (if m then id else AppE (ConE 'Identity)) $
                    (if o then id else AppE (ConE 'Just)) $
                        (`AppE` VarE x) $
                            (if i then (`AppE` (VarE j)) else id) $
                                VarE f
                )
     in
        return
            [ PragmaD (InlineP n Inline FunLike AllPhases),
              FunD
                n
                [ Clause
                    (VarP f : VarP xs : _if s [VarP ys])
                    ( NormalB
                        ( (if not m then AppE (VarE 'runIdentity) else id)
                            (VarE base `AppE` f' `AppE` VarE xs `AppE` (if s then VarE ys else ConE 'Empty))
                        )
                    )
                    []
                ]
            ]

mkAllMaps :: Q [Dec]
mkAllMaps = mk
  where
    tf = [True, False]
    mk = fmap concat $ forM (tf <&> tf <.&> tf <..&> tf <.:&> tf <..:&> tf) \case
        (True, True, True, True, _, _) -> return []
        (True, _, _, _, False, True) -> return []
        (False, True, True, True, False, True) -> return []
        (i, o, m, s, r, b) -> mkOneMap i o m s r b

{- | Generates span* variants
    $1: in situ
    $2: with Index
    $3: with monad
    $4: L/R
-}
mkOneSpan :: Bool -> Bool -> Bool -> Bool -> Q [Dec]
mkOneSpan s i m l =
    let
        name = mkName $ "span" ++ _if s "s" ++ _if i "Ix" ++ (if l then "L" else "R") ++ _if m "M"
        base = mkName $ "spansIx" ++ (if l then "L" else "R") ++ "M"
        f = mkName "f"
        xs = mkName "xs"
        ys = mkName "ys"
        x = mkName "x"
        j = mkName "i"
        mT = mkName "m"
        tv a = PlainTV a SpecifiedSpec
        mW t = if m then VarT mT `AppT` t else t
        t =
            ForallT
                ( _if i [tv j]
                    ++ _if m [tv mT]
                    ++ [tv x, tv xs, tv ys]
                )
                ( _if i [ConT ''Enum `AppT` VarT j]
                    ++ _if (not s) [ConT ''AsEmpty `AppT` VarT ys]
                    ++ _if m [ConT ''Monad `AppT` VarT mT]
                    ++ [ ConT (if l then ''Cons else ''Snoc) `AppT` VarT ys `AppT` VarT ys `AppT` VarT x `AppT` VarT x,
                         ConT ''Uncons `AppT` VarT xs `AppT` VarT x
                       ]
                )
                ( fun
                    ( fun (_if i [VarT j] ++ [VarT x]) (mW (ConT ''Bool))
                        : VarT xs
                        : _if s [VarT ys]
                    )
                    (mW (tup [VarT ys, VarT xs]))
                )
        f' =
            LamE
                ((if i then VarP j else SigP WildP (ConT ''Int)) : [VarP x])
                ( (if m then id else AppE (ConE 'Identity)) $
                    (`AppE` VarE x) $
                        (if i then (`AppE` (VarE j)) else id) $
                            VarE f
                )
     in
        return
            [ SigD name t,
              PragmaD (InlineP name Inline FunLike AllPhases),
              FunD
                name
                [ Clause
                    (VarP f : VarP xs : _if s [VarP ys])
                    ( NormalB
                        ( (if not m then AppE (VarE 'runIdentity) else id)
                            (VarE base `AppE` f' `AppE` VarE xs `AppE` (if s then VarE ys else ConE 'Empty))
                        )
                    )
                    []
                ]
            ]

mkAllSpans :: Q [Dec]
mkAllSpans = mk
  where
    tf = [True, False]
    mk = fmap concat $ forM (tf <&> tf <.&> tf <..&> tf) \case
        (True, True, True, _) -> return []
        (s, i, m, l) -> mkOneSpan s i m l
