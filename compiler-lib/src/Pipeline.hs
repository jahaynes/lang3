module Pipeline ( compile
                ) where

import Core.Expression          (Expr)
import Core.Module              (Module)
import Transform.Anf            (NModule, anf)
import Transform.ClosureConvert (closureConvert)
import Transform.Hoist          (hoist)
import TypeSystem.InferModule   (inferModule)
import TypeSystem.Types         (DataDefn, Type)

import Data.String (IsString)

compile :: (IsString s, Ord s, Show s) => [DataDefn s]
        -> Module (Expr () s) s
        -> Either s (NModule (Type s) s)
compile dataDefns md = do
    typed   <- inferModule dataDefns md
    cc      <- closureConvert typed
    hoisted <- hoist cc
    anf hoisted
