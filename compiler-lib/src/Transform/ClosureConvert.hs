module Transform.ClosureConvert ( closureConvert
                                ) where

import Core.Expression (Expr (..))
import Core.Module     (Fun (..), Module (..))

closureConvert :: Monad m => Module t s -> m (Module t s)
closureConvert md = do
    let funDefs = getFunDefns md
    Module <$> traverse ccFun funDefs

ccFun :: Monad m => Fun t s -> m (Fun t s)
ccFun (Fun n e) = Fun n <$> ccExpr e

ccExpr :: Monad m => Expr t s -> m (Expr t s)
ccExpr e = pure e