module TypeSystem.TypesCallGraph ( buildModuleGraph
                                 , freeVars
                                 ) where

import Core.Expression     (Expr (..), Term (..), patternVars)
import Core.Module         (Fun (..), Module (..))
import TypeSystem.CallGraph

import qualified Data.Map.Strict as M
import           Data.Set        (Set, (\\))
import qualified Data.Set as S

-- | Which top-level functions each top-level function refers to (not
-- counting itself). References to anything else -- locals, or names that
-- aren't bound at all -- are left out: those are inference's business.
buildModuleGraph :: Ord s => Module (Expr t s) s -> CallGraph s
buildModuleGraph md =
    CallGraph $ M.fromList [ (n, S.delete n (freeVars e) `S.intersection` globals)
                           | Fun n e <- funs ]
    where
    funs    = getFunDefns md
    globals = S.fromList [ n | Fun n _ <- funs ]

freeVars :: Ord s => Expr t s -> Set s
freeVars = \case
    Term (Var _ v)    -> S.singleton v
    Term _            -> mempty
    App _ f xs        -> foldMap freeVars (f : xs)
    Lam _ vs body     -> freeVars body \\ S.fromList vs
    Let bs body       -> (foldMap (\(_, _, e) -> freeVars e) bs <> freeVars body)
                             \\ S.fromList [ n | (_, n, _) <- bs ]
    DataCon _ _ xs    -> foldMap freeVars xs
    Case _ scrut alts -> freeVars scrut
                      <> foldMap (\(p, e) -> freeVars e \\ S.fromList (patternVars p)) alts
    BinOp _ _ a b     -> freeVars a <> freeVars b
