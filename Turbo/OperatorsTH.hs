{-# OPTIONS_HADDOCK hide #-}

module Turbo.OperatorsTH (makeApplicativeWrapper, makeLeftWrapper, makeOperators, makeRightWrapper, makePostponed, makeFuncSubst, makeTuplePaste, makeCoincideSubst) where

import Data.Char (isUpper)
import Data.Foldable
import GHC.Err (error)
import Language.Haskell.TH
import Turbo.Extra.TH ((→))
import Turbo.RootPrelude

-- | The context placed on a type signature
type OpInfo = (Name, Cxt, Type, Type, Type)

-- | Whether a type is equivalent to (->) :: * -> * -> *
isArrow :: Type -> Bool
isArrow ArrowT = True
isArrow (AppT MulArrowT _) = True
isArrow _ = False

-- | generates an inline pragma for the given function name
mkInline :: Name -> Dec
mkInline nm = PragmaD (InlineP nm Inline FunLike AllPhases)

-- | decodes a binary function's types from its name
opInfo :: Name -> Q OpInfo
opInfo x = do
    r <- reify x
    let ft = case r of
            VarI _ t _ -> t
            DataConI _ t _ -> t
            _ -> error "Expected a regular function"
    let (ctx, t) = case ft of
            ForallT _ c t -> (c, t)
            t -> ([], t)
    case t of
        -- (->) a ((->) b c)   === a -> (b -> c)
        AppT (AppT p a) (AppT (AppT q b) c) | isArrow q, isArrow p -> return (x, ctx, a, b, c)
        _ -> error $ "Expected a binary function, got: " ++ show t

-- | Gets an expression for a constructor or normal function by its name
conOrVarE :: Name -> Q Exp
conOrVarE n = case nameBase n of
    ':' : _ -> conE n
    c : _ | isUpper c -> conE n
    _ -> varE n

-- | creates a left-sided functor wrapper for an existing operator
leftWrapper :: OpInfo -> Q [Dec]
leftWrapper (n, ctx, a, b, c) = do
    let n' = mkName $ '<' : nameBase n
    f <- newName "f"
    let fx = Fixity 4 InfixL
    let a' = AppT (VarT f) a
    let c' = AppT (VarT f) c
    let ctx' = (ConT ''Functor) `AppT` (VarT f) : ctx
    v <- [e|\a b -> fmap (\x -> $(conOrVarE n) x b) a|]
    return
        [ InfixD fx n',
          SigD n' (ForallT [] ctx' (a' → b → c')),
          mkInline n',
          FunD n' [Clause [] (NormalB v) []]
        ]

-- | creates a right-sided functor wrapper for an existing operator
rightWrapper :: OpInfo -> Q [Dec]
rightWrapper (n, ctx, a, b, c) = do
    let n' = mkName $ nameBase n ++ ">"
    f <- newName "f"
    let fx = Fixity 4 InfixL
    let b' = AppT (VarT f) b
    let c' = AppT (VarT f) c
    let ctx' = (ConT ''Functor) `AppT` (VarT f) : ctx
    v <- [e|fmap . $(conOrVarE n)|]
    return
        [ InfixD fx n',
          SigD n' (ForallT [] ctx' (a → b' → c')),
          mkInline n',
          FunD n' [Clause [] (NormalB v) []]
        ]

-- | creates a two-sided applicative wrapper for an existing operator
applicativeWrapper :: OpInfo -> Q [Dec]
applicativeWrapper (n, ctx, a, b, c) = do
    let n' = mkName $ '<' : nameBase n ++ ">"
    f <- newName "f"
    let fx = Fixity 4 InfixL
    let a' = AppT (VarT f) a
    let b' = AppT (VarT f) b
    let c' = AppT (VarT f) c
    let ctx' = (ConT ''Applicative) `AppT` (VarT f) : ctx
    v <- [e|liftA2 $(conOrVarE n)|]
    return
        [ InfixD fx n',
          SigD n' (ForallT [] ctx' (a' → b' → c')),
          mkInline n',
          FunD n' [Clause [] (NormalB v) []]
        ]

-- | Makes the operator `<·` from `·`
makeLeftWrapper :: Name -> Q [Dec]
makeLeftWrapper n = opInfo n >>= leftWrapper

-- | Makes the operator `·>` from `·`
makeRightWrapper :: Name -> Q [Dec]
makeRightWrapper n = opInfo n >>= rightWrapper

-- | Makes the operator `<·>` from `·`
makeApplicativeWrapper :: Name -> Q [Dec]
makeApplicativeWrapper n = opInfo n >>= applicativeWrapper

-- | Makes the operators `<·`, `·>` and `<·>` from `·`
makeOperators :: Name -> Q [Dec]
makeOperators n = do
    o <- opInfo n
    as <- leftWrapper o
    bs <- rightWrapper o
    cs <- applicativeWrapper o
    return $ as ++ bs ++ cs

-- | Generates a dot decoration for the left side of an operator
dotPrefix :: Word -> String
dotPrefix 0 = ""
dotPrefix n = let m = n `div` 2 in if even n then ".." ++ (replicate (m - 1) ':') else '.' : replicate m ':'

-- | Generates a dot decoration for the right side of an operator
dotSuffix :: Word -> String
dotSuffix 0 = ""
dotSuffix n = replicate (n `div` 2) ':' ++ if odd n then "." else ""

-- | shorthand for generating a function type with arguments $1 and return $2
funT :: [Type] -> Type -> Type
funT as r = foldr (→) r as

-- | shorthand for generating a function type directly from type names
funT' :: [Name] -> Name -> Type
funT' as r = funT (fmap VarT as) (VarT r)

-- | shorthand for generating tuple types that avoids introducing `Solo`s
tupT :: [Type] -> Type
tupT [x] = x
tupT fs = foldl AppT (TupleT (length fs)) fs

-- | shorthand for generating tuple types from names that avoids introducing `Solo`s
tupT' :: [Name] -> Type
tupT' = tupT . fmap VarT

-- | Shorthand for generating multiple new variables
newNames :: Word -> String -> Q [Name]
newNames n x = mapM newName (replicate n x)

-- | shorthand for applying a function to variables via their names
app' :: Name -> [Name] -> Exp
app' f xs = foldl AppE (VarE f) (fmap VarE xs)

-- | Generates every whole-valued pair so that `n+m  =  $1`
unSum :: (Ord n, Num n) => n -> [(n, n)]
unSum n = ns 0
  where
    ns m | m > n = []
    ns m = (m, n - m) : ns (m + 1)

-- | Generates a tuple pattern and avoids introducing `Solo`
tupP' :: [Name] -> Pat
tupP' [n] = VarP n
tupP' ns = TupP (fmap VarP ns)

-- | Generates an operator for postponed `$` with $1 dollars and $2 dots
postponedDollar :: Word -> Word -> Q [Dec]
postponedDollar m n = do
    args <- mapM newName (replicate n "a")
    tuple@(x0 : _) <- mapM newName (replicate m "x")
    fn <- newName "f"
    ret <- newName "y"
    let nm = mkName (dotPrefix n ++ replicate m '$')
    let t1 = funT' (args ++ tuple) ret
    let t2 = if m == 1 then VarT x0 else tupT' tuple
    let t3 = funT' args ret
    let o = (nm, [], t1, t2, t3) :: OpInfo
    as <- leftWrapper o
    bs <- rightWrapper o
    cs <- applicativeWrapper o
    return $
        [ InfixD (Fixity 0 InfixR) nm,
          SigD nm (t1 → t2 → t3),
          mkInline nm,
          FunD
            nm
            [ Clause
                (VarP fn : (if m == 1 then VarP x0 else TupP (fmap VarP tuple)) : fmap VarP args)
                ( NormalB $
                    foldl AppE (VarE fn) (fmap VarE (args ++ tuple))
                )
                []
            ]
        ]
            ++ as
            ++ bs
            ++ cs

makePostponed :: Word -> [Word] -> Q [Dec]
makePostponed n ms = fmap concat $ mapM (postponedDollar n) ms

-- | Generates a °-operator for substituting the $1'th argument to the left function with the arity $2 right function
funcSubst :: Word -> Word -> Q [Dec]
funcSubst n m = do
    let nm = mkName $ dotPrefix n ++ "°" ++ dotSuffix m
    xs <- newNames n "x"
    ys <- newNames (m + 1) "y"
    sub <- newName "a"
    res <- newName "b"
    let f1 = funT' (xs ++ [sub]) res
    let f2 = funT' ys sub
    let fo = funT' (xs ++ ys) res
    let o = (nm, [], f1, f2, fo) :: OpInfo
    l <- leftWrapper o
    r <- rightWrapper o
    a <- applicativeWrapper o
    let f = mkName "f"
    let g = mkName "g"
    return $
        [ InfixD (Fixity 9 InfixR) nm,
          SigD nm (f1 → f2 → fo),
          mkInline nm,
          FunD
            nm
            [ Clause
                (VarP f : VarP g : fmap VarP (xs ++ ys))
                ( NormalB $
                    app' f xs `AppE` app' g ys
                )
                []
            ]
        ]
            ++ l
            ++ r
            ++ a

makeFuncSubst :: Word -> Q [Dec]
makeFuncSubst n = fmap concat $ forM (unSum n) (uncurry funcSubst)

-- | Generates an &-operator for combining a ($1+1) tuple and an ($2+1) tuple together
tuplePaste :: Word -> Word -> Q [Dec]
tuplePaste n m = do
    let nm = mkName $ dotPrefix n ++ "&" ++ dotSuffix m
    ls <- newNames (1 + n) "x"
    rs <- newNames (1 + m) "y"
    let t1 = tupT' ls
    let t2 = tupT' rs
    let to = tupT' (ls ++ rs)
    let o = (nm, [], t1, t2, to) :: OpInfo
    l <- leftWrapper o
    r <- rightWrapper o
    a <- applicativeWrapper o
    return $
        [ InfixD (Fixity 1 InfixL) nm,
          SigD nm (t1 → t2 → to),
          mkInline nm,
          FunD
            nm
            [ Clause [tupP' ls, tupP' rs] (NormalB $ TupE $ fmap (Just . VarE) (ls ++ rs)) []
            ]
        ]
            ++ l
            ++ r
            ++ a

makeTuplePaste :: Word -> Q [Dec]
makeTuplePaste n = fmap concat $ forM (unSum n) (uncurry tuplePaste)

-- | Generates a `°´`-operator for combining an arity ($1+$2+1) function
--   with a ($1+1) function that returns ($2+1) values, where types match position-wise
coincideSubst :: Word -> Word -> Q [Dec]
coincideSubst n m = do
    let nm = mkName $ dotPrefix n ++ "°" ++ dotSuffix m ++ "´"
    let f1 = mkName "f1"
    let f2 = mkName "f2"
    let r = mkName "r"
    -- arguments that f1 and f2 have in common
    xs <- newNames (1 + n) "x"
    -- arguments that only f1 accepts and f2 returns
    ys <- newNames (1 + m) "y"
    let t1 = funT' (xs ++ ys) r
    let t2 = funT (map VarT xs) (tupT' ys)
    let t3 = funT' xs r 
    let o = (nm, [], t1, t2, t3)
    l <- leftWrapper o
    r <- rightWrapper o
    a <- applicativeWrapper o
    return $ [
        InfixD (Fixity 9 InfixR) nm,
        SigD nm (t1 → t2 → t3),
        mkInline nm,
        FunD nm [
            Clause (VarP f1 : VarP f2 : (map VarP xs)) (NormalB $ LetE [
                ValD (tupP' ys) (NormalB $ app' f2 xs) []
             ] (app' f1 (xs ++ ys))) []
        ]
     ] ++ l ++ r ++ a

makeCoincideSubst :: Word -> Q [Dec]
makeCoincideSubst n = fmap concat $ forM (unSum n) (uncurry coincideSubst)
