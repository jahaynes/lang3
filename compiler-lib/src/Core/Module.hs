module Core.Module ( Fun (..)
                   , Module (..)
                   ) where

import Core.Expression (Expr)

data Module t s =
    Module { getFunDefns :: ![Fun t s]
           }

data Fun t s =
    Fun !s !(Expr t s)
