module Core.Expression ( Expr (..)
                       , Term (..)
                       ) where

data Expr t s
    = Term !(Term t s)
    | App !t !(Expr t s) ![Expr t s]
    | Lam !t ![s] !(Expr t s)
    | Let !t !s !(Expr t s) !(Expr t s)
        deriving Show

data Term t s
    = Var !t !s
    | LitInt !Int
    | LitBool !Bool
    | LitString !s
        deriving Show