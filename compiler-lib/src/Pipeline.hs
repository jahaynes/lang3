module Pipeline ( compile
                ) where

import Core.Expression          (Expr)
import Core.Module              (Module)
import Transform.Anf            (NModule, anf)
import Transform.ClosureConvert (closureConvert)
import Transform.EtaSaturate    (etaSaturate)
import Transform.Hoist          (hoist)
import Transform.Monomorphise   (monomorphise)
import TypeSystem.InferModule   (inferModule)
import TypeSystem.Types         (MType)

import Data.String (IsString)

compile :: (IsString s, Ord s, Show s)
        => Module (Expr () s) s
        -> Either s (NModule (MType s) s)
compile md = do
    typed   <- inferModule md
    mono    <- monomorphise typed
    sat     <- etaSaturate mono
    cc      <- closureConvert sat
    hoisted <- hoist cc
    anf hoisted
