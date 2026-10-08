module Transform.ClosureConvert ( closureConvert
                                ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Core.Expression (Expr (..))
import Core.Module     (Fun (..), Module (..))

data Scope = Scope

closureConvert :: Monad m => Module t s -> m (Module t s)
closureConvert md = do

    let funDefs = getFunDefns md

    funDefs' <- runReaderT (traverse ccFun funDefs) Scope

    pure Module { getFunDefns = funDefs' }

ccFun :: Monad m
      => Fun t s -> ReaderT Scope m (Fun t s)
ccFun (Fun n e) = Fun n <$> ccExpr e

ccExpr :: Monad m
       => Expr t s -> ReaderT Scope m (Expr t s)
ccExpr e = pure e
