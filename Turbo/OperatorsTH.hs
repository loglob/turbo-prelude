{-# OPTIONS_HADDOCK hide #-}
module Turbo.OperatorsTH (makeApplicativeWrapper, makeLeftWrapper, makeOperators, makeRightWrapper) where
import Language.Haskell.TH
import Turbo.RootPrelude
import Data.Char (isUpper)
import GHC.Err (error)
import Turbo.ExtraTH ((→))

-- | The context placed on a type signature
type OpInfo = (Name, Fixity, Cxt, Type, Type, Type)

-- | Whether a type is equivalent to (->) :: * -> * -> *
isArrow :: Type -> Bool
isArrow ArrowT = True
isArrow (AppT MulArrowT _) = True
isArrow _ = False

mkInline :: Name -> Dec
mkInline nm = PragmaD (InlineP nm Inline FunLike AllPhases)

-- | decodes a binary function's types from its name
opInfo :: Name -> Q OpInfo
opInfo x = do
    r <- reify x
    Just f <- reifyFixity x
    let ft = case r of
            VarI _ t _ -> t
            DataConI _ t _ -> t
            _ -> error "Expected a regular function"
    let (ctx,t) = case ft of
           ForallT  _ c t -> (c, t)
           t              -> ([], t)
    case t of
        -- (->) a ((->) b c)   === a -> (b -> c)
        AppT (AppT p a) (AppT (AppT q b) c) | isArrow q, isArrow p -> return (x, f, ctx, a, b, c)
        _ -> error$ "Expected a binary function, got: " ++ show t

-- | Gets an expression for a constructor or normal function by its name 
conOrVarE :: Name -> Q Exp
conOrVarE n = case nameBase n of
    ':':_ -> conE n
    c:_   | isUpper c -> conE n
    _     -> varE n

leftWrapper :: OpInfo -> Q [Dec]
leftWrapper (n,_,ctx,a,b,c) = do
    let n' = mkName$ '<' : nameBase n
    f <- newName "f"
    let fx = Fixity 4 InfixL
    let a' = AppT (VarT f) a
    let c' = AppT (VarT f) c
    let ctx' = (ConT ''Functor) `AppT` (VarT f) : ctx
    v <- [e| \a b -> fmap (\x -> $(conOrVarE n) x b) a |]
    return [
        InfixD fx n' ,
        SigD n' (ForallT [] ctx' (a' → b → c')) ,
        mkInline n' ,
        FunD n' [ Clause [] (NormalB v) [] ]
     ]

rightWrapper :: OpInfo -> Q [Dec]
rightWrapper (n,_,ctx,a,b,c) = do
    let n' = mkName$ nameBase n ++ ">"
    f <- newName "f"
    let fx = Fixity 4 InfixL
    let b' = AppT (VarT f) b
    let c' = AppT (VarT f) c
    let ctx' = (ConT ''Functor) `AppT` (VarT f) : ctx
    v <- [e| fmap . $(conOrVarE n) |]
    return [
        InfixD fx n' ,
        SigD n' (ForallT [] ctx' (a → b' → c')) ,
        mkInline n' ,
        FunD n' [ Clause [] (NormalB v) [] ]
     ]

applicativeWrapper :: OpInfo -> Q [Dec]
applicativeWrapper (n,_,ctx,a,b,c) = do
    let n' = mkName$ '<' : nameBase n ++ ">"
    f <- newName "f"
    let fx = Fixity 4 InfixL
    let a' = AppT (VarT f) a
    let b' = AppT (VarT f) b
    let c' = AppT (VarT f) c
    let ctx' = (ConT ''Applicative) `AppT` (VarT f) : ctx
    v <- [e| liftA2 $(conOrVarE n) |]
    return [
        InfixD fx n' ,
        SigD n' (ForallT [] ctx' (a' → b' → c')) ,
        mkInline n' ,
        FunD n' [ Clause [] (NormalB v) [] ]
     ]

-- | makes the operator `<· :: Functor f => f a -> b -> f c` from `· :: a -> b -> c`
makeLeftWrapper :: Name -> Q [Dec]
makeLeftWrapper n = opInfo n >>= leftWrapper

-- | makes the operators `·>`, `·>>`, `·>>>` and `·>>>>` from ·
--   Introduces a `Functor` on the right argument
makeRightWrapper :: Name -> Q [Dec]
makeRightWrapper n = opInfo n >>= rightWrapper

-- | makes operators like `<·>`, `<<·>` or `<·>>>` from ·
--   These wrap both sides in an `Applicative` type
makeApplicativeWrapper :: Name -> Q [Dec]
makeApplicativeWrapper n = opInfo n >>= applicativeWrapper

-- | makes operators like `<·`, `<<·`, `·>` and `<·>` from ·
makeOperators :: Name -> Q [Dec]
makeOperators n = do
    o <- opInfo n
    as <- leftWrapper o
    bs <- rightWrapper o
    cs <- applicativeWrapper o
    return$ as ++ bs ++ cs
