module Transform.ClosureConvert ( CCExpr (..)
                                , closureConvert
                                ) where

import Core.Expression (Expr (..), Term (..))
import Core.Module     (Fun (..), Module (..))
import Transform.Class

import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Set                   (Set)
import qualified Data.Set as S

newtype Scope s
    = Scope (Set s)

data CCExpr t s
    = CCTerm !(Term t s)
    | CCApp !t !(CCExpr t s) ![CCExpr t s]
    | CCLet !t !s !(CCExpr t s) !(CCExpr t s)

closureConvert :: (ClosureConvert a, Monad m, Ord s)
               => Module (Expr t s) s -> m (Module (a t s) s)
closureConvert md = do

    let funDefs  = getFunDefns md
        funNames = map (\(Fun n _) -> n) funDefs
        scope    = Scope (S.fromList funNames)

    funDefs' <- runReaderT (traverse ccFun funDefs) scope

    pure Module { getFunDefns = funDefs' }

ccFun :: (ClosureConvert a, Monad m)
      => Fun (Expr t s) s -> ReaderT (Scope s) m (Fun (a t s) s)
ccFun (Fun n e) = Fun n <$> ccExpr e

ccExpr :: (ClosureConvert a, Monad m)
       => Expr t s -> ReaderT (Scope s) m (a t s)
ccExpr expr =

    case expr of
        
        Term t ->
            pure $ term t

        App ty f xs ->
            app ty <$> ccExpr f
                   <*> traverse ccExpr xs

        Lam{} ->
            undefined

        Let ty a b c ->
            lett ty a <$> ccExpr b
                      <*> ccExpr c

class (App a, CTerm a, Let a)
    => ClosureConvert a

instance App CCExpr where
    app = CCApp

instance CTerm CCExpr where
    term = CCTerm

instance Let CCExpr where
    lett = CCLet