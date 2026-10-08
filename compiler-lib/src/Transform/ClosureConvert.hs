module Transform.ClosureConvert ( closureConvert
                                ) where

import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Core.Expression (Expr (..))
import Core.Module     (Fun (..), Module (..))

import           Data.Set      (Set)
import qualified Data.Set as S

newtype Scope s
    = Scope (Set s)

closureConvert :: (Monad m, Ord s)
               => Module t s -> m (Module t s)
closureConvert md = do

    let funDefs  = getFunDefns md
        funNames = map (\(Fun n _) -> n) funDefs
        scope    = Scope (S.fromList funNames)

    funDefs' <- runReaderT (traverse ccFun funDefs) scope

    pure Module { getFunDefns = funDefs' }

ccFun :: Monad m
      => Fun t s -> ReaderT (Scope s) m (Fun t s)
ccFun (Fun n e) = Fun n <$> ccExpr e

ccExpr :: Monad m
       => Expr t s -> ReaderT (Scope s) m (Expr t s)
ccExpr e = pure e
