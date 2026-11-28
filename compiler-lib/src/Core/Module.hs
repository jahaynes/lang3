module Core.Module ( Fun (..)
                   , Module (..)
                   , TypeSig (..)
                   ) where

import TypeSystem.Types (DataDefn, Type)

data Module a s =
    Module { getDataDefns :: ![DataDefn s]
           , getTypeSigs  :: ![TypeSig s]
           , getFunDefns  :: ![Fun a s]
           } deriving (Eq, Show)

data TypeSig s =
    TypeSig !s !(Type s)
        deriving (Eq, Show)

data Fun a s =
    Fun !s !a
        deriving (Eq, Show)
