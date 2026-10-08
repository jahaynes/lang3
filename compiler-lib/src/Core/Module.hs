module Core.Module ( Fun (..)
                   , Module (..)
                   ) where

data Module a s =
    Module { getFunDefns :: ![Fun a s]
           }

data Fun a s =
    Fun !s !a
