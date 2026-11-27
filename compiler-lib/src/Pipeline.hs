module Pipeline ( compile
                ) where

import Core.Expression          (Expr)
import Core.Module              (Module)
import Transform.Anf            (NModule, anf)
import Transform.ClosureConvert (closureConvert)
import Transform.Hoist          (hoist)
import Transform.Monomorphise   (monomorphise)
import TypeSystem.InferModule   (inferModule)
import TypeSystem.Types         (DataDefn, MType)

import Data.String (IsString)

compile :: (IsString s, Ord s, Show s) => [DataDefn s]
        -> Module (Expr () s) s
        -> Either s (NModule (MType s) s)
compile dataDefns md = do
    typed   <- inferModule dataDefns md
    mono    <- monomorphise typed
    cc      <- closureConvert mono
    hoisted <- hoist cc
    anf hoisted
