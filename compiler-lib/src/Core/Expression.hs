module Core.Expression ( Expr (..)
                       , Term (..)
                       ) where

data Expr t s
    = Term !t !(Term t s)
    | App !t !(Expr t s) ![Expr t s]
    | Lam !t ![s] !(Expr t s)
    | Let !t !s !(Expr t s) !(Expr t s)

data Term t s
    = Var !t !s
    | LitInt !t !Int
    | LitBool !t !Bool
