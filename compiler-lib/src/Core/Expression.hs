module Core.Expression ( Expr (..)
                       , Op (..)
                       , Pattern (..)
                       , Term (..)
                       , patternVars
                       ) where

data Expr t s
    = Term !(Term t s)
    | App !t !(Expr t s) ![Expr t s]
    | Lam !t ![s] !(Expr t s)
    | Let ![(t, s, Expr t s)] !(Expr t s)
    | DataCon !t !s ![Expr t s]
    | Case !t !(Expr t s) ![(Pattern s, Expr t s)]
    | BinOp !t !Op !(Expr t s) !(Expr t s)
        deriving (Eq, Show)

data Op = Plus | Times | Equals | And | Or
    deriving (Eq, Ord, Show)

data Term t s
    = Var !t !s
    | LitInt !Int
    | LitBool !Bool
    | LitString !s
        deriving (Eq, Show)

data Pattern s
    = PLitInt !Int
    | PLitBool !Bool
    | PVar !s
    | PDataCon !s ![Pattern s]
    | PWild
        deriving (Eq, Show)

patternVars :: Pattern s -> [s]
patternVars = \case
    PVar v        -> [v]
    PDataCon _ ps -> concatMap patternVars ps
    _             -> []
