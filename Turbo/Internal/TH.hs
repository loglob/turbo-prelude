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
        y = mkName "y"
        j = mkName "i"
        mT = mkName "m"
        mW t = if m then VarT mT `AppT` t else t
        t =
            ForallT
                ( _if i [tv j]
                    ++ _if m [tv mT]
                    ++ [tv y, tv x, tv xs, tv ys]
                )
                ( _if i [ConT ''Enum `AppT` VarT j]
                    ++ _if (not s) [ConT ''AsEmpty `AppT` VarT ys]
                    ++ _if m [ConT ''Monad `AppT` VarT mT]
                    ++ [ ConT (if r then ''Uncons else ''Unsnoc) `AppT` VarT xs `AppT` VarT x,
                         ConT (if b then ''Cons else ''Snoc) `AppT` VarT ys `AppT` VarT ys `AppT` VarT y `AppT` VarT y,
                         ConT ''Uncons `AppT` VarT xs `AppT` VarT x
                       ]
                )
                ( fun
                    ( fun (_if i [VarT j] ++ [VarT x]) (mW ((if o then (ConT ''Maybe `AppT`) else id) (VarT y)))
                        : VarT xs
                        : _if s [VarT ys]
                    )
                    (mW (VarT ys))
                )
        f' =
            LamE
                (_if regular [if i then VarP j else SigP WildP (ConT ''Int)] ++ [VarP x])
                ( (if m then id else AppE (ConE 'Identity)) $
                    (if o then id else if m then AppE (VarE 'fmap `AppE` ConE 'Just) else AppE (ConE 'Just)) $
                        (`AppE` VarE x) $
                            (if i then (`AppE` (VarE j)) else id) $
                                VarE f
                )
     in
        return
            [ SigD n t,
              PragmaD (InlineP n Inline FunLike AllPhases),
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
    $1: is *End*
    $2: in situ
    $3: with Index
    $4: with monad
    $5: L/R
-}
mkOneSpan :: Bool -> Bool -> Bool -> Bool -> Bool -> Q [Dec]
mkOneSpan e s i m l =
    let
        name = mkName $ "span" ++ _if s "s" ++ _if e "End" ++ _if i "Ix" ++ (if l then "L" else "R") ++ _if m "M"
        base = mkName $ "spans" ++ (if e then "End" else "Ix") ++ (if l then "L" else "R") ++ "M"
        f = mkName "f"
        xs = mkName "xs"
        ys = mkName "ys"
        x = mkName "x"
        j = mkName "i"
        mT = mkName "m"
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
                         ConT (if e then ''Unsnoc else ''Uncons) `AppT` VarT xs `AppT` VarT x
                       ]
                )
                ( fun
                    ( fun (_if i [VarT j] ++ [VarT x]) (mW (ConT ''Bool))
                        : VarT xs
                        : _if s [VarT ys]
                    )
                    (mW (tup $ (if e then reverse else id) [VarT ys, VarT xs]))
                )
        f' =
            LamE
                (_if (not e) [if i then VarP j else SigP WildP (ConT ''Int)] ++ [VarP x])
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
    mk = fmap concat $ forM (tf <&> tf <.&> tf <..&> tf <.:&> tf) \case
        (e, True, i, True, _) | e /= i -> return []
        (True, _, True, _, _) -> return []
        (e, s, i, m, l) -> mkOneSpan e s i m l
