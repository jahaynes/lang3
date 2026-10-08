module Transform.ClosureConvert ( CCExpr (..)
                                , closureConvert
                                ) where

import Core.Expression (Expr (..))
import Core.Module     (Fun (..), Module (..))
import Transform.Class (App (..))

import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Set                   (Set)
import qualified Data.Set as S

newtype Scope s
    = Scope (Set s)

data CCExpr t s
    = CCApp !t !(CCExpr t s) ![CCExpr t s]

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
        
        Term{} ->
            undefined

        App t f xs ->
            app t <$> ccExpr f
                  <*> traverse ccExpr xs

        Lam{} ->
            undefined

        Let{} ->
            undefined
    
class (App a) => ClosureConvert a

instance App CCExpr where
    app = CCApp
