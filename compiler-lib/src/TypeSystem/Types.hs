{-# LANGUAGE DeriveFunctor #-}

module TypeSystem.Types ( DataDefn (..)
                        , DCon (..)
                        , Polytype (..)
                        , Type (..)
                        , arity
                        , renderName
                        , renderType
                        , tyArr
                        , typeBool
                        , typeInt
                        , typeString
                        ) where

import Data.List   (intercalate)
import Data.String (IsString)

-- | Type application is curried: @Either a b@ is
-- @TyApp (TyApp (TyCon "Either") a) b@, and 'TyCon' is always nullary.
--
-- Arrows are n-ary: a function's arity is part of its type, so
-- @TyArr [a, b] c@ (takes two arguments at once) and
-- @TyArr [a] (TyArr [b] c)@ (takes one, returning a function that takes
-- one) are different types, and don't unify. A 'TyArr' never has an empty
-- parameter list: build arrows with 'tyArr', which keeps that invariant.
data Type s = TyVar s
            | TyCon s
            | TyArr ![Type s] !(Type s)
            | TyApp !(Type s) !(Type s)
                deriving (Eq, Ord, Functor)

-- | An arrow from the given parameters. With none, it's just the result
-- type: @\\ -> e@ and @e []@ have @e@'s type.
tyArr :: [Type s] -> Type s -> Type s
tyArr [] r = r
tyArr ps r = TyArr ps r

-- | How many arguments a value of this type takes at once: 0 for anything
-- but an arrow.
arity :: Type s -> Int
arity (TyArr ps _) = length ps
arity _            = 0

instance Show s => Show (Type s) where
    show (TyVar v) = show v
    show (TyCon c) = show c
    show (TyArr ps r) = "(" ++ showParams (map show ps) ++ " -> " ++ show r ++ ")"
    show (TyApp t1 t2) = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

showParams :: [String] -> String
showParams [p] = p
showParams ps  = "(" ++ intercalate ", " ps ++ ")"

-- | A type as it reads in an error message: @(Int, Int) -> List a@.
-- Names are shown without the quotes 'show' gives a String.
renderType :: Show s => Type s -> String
renderType = go 0
    where
    go :: Show s => Int -> Type s -> String
    go _ (TyVar v)    = renderName v
    go _ (TyCon c)    = renderName c
    go p (TyApp f x)  = parensIf (p > 1) (go 1 f ++ " " ++ go 2 x)
    go p (TyArr ps r) = parensIf (p > 0) (params ps ++ " -> " ++ go 0 r)

    params [t] = go 1 t
    params ts  = "(" ++ intercalate ", " (map (go 0) ts) ++ ")"

    parensIf True  s = "(" ++ s ++ ")"
    parensIf False s = s

-- | A name as it reads in an error message, without the quotes 'show'
-- gives a String or ByteString.
renderName :: Show s => s -> String
renderName = filter (/= '"') . show

data Polytype s =
    Forall [s] (Type s)
        deriving (Eq, Show)

-- | A data type declaration: its name, its type parameters, and its
-- constructors. Every constructor is quantified over exactly the declared
-- parameters (see 'TypeSystem.InferModule.dataDefnToType').
data DataDefn s =
    DataDefn !s ![s] ![DCon s]
        deriving (Eq, Show)

-- | A data constructor and its field types.
data DCon s =
    DCon !s ![Type s]
        deriving (Eq, Show)

typeBool, typeInt, typeString :: IsString s => Type s
typeBool   = TyCon "Bool"
typeInt    = TyCon "Int"
typeString = TyCon "String"
