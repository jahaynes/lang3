module Transform.ClosureConvert ( CCExpr (..)
                                , closureConvert
                                ) where

import Core.Expression (Expr (..), Term (..))
import Core.Module     (Fun (..), Module (..))
import Transform.Class

import           Control.Monad.Trans.Reader (ReaderT, ask, local, runReaderT)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict as M
import           Data.Set                   (Set, (\\))
import qualified Data.Set as S

data Binding
    = Global
    | Lifted
    | Local

newtype Scope s
    = Scope (Map s Binding)

data CCExpr t s
    = CCTerm !(Term t s)
    | CCApp !t !(CCExpr t s) ![CCExpr t s]
    | CCClosure !t !(Set s) ![s] !(CCExpr t s)
    | CCLet !t !s !(CCExpr t s) !(CCExpr t s)
        deriving Show

closureConvert :: (ClosureConvert a, Monad m, Ord s)
               => Module (Expr t s) s -> m (Module (a t s) s)
closureConvert md = do

    let funDefs  = getFunDefns md
        funNames = map (\(Fun n _) -> n) funDefs
        scope    = Scope (M.fromList [(n, Global) | n <- funNames])

    funDefs' <- runReaderT (traverse ccFun funDefs) scope

    pure Module { getFunDefns = funDefs' }

ccFun :: (ClosureConvert a, Monad m, Ord s)
      => Fun (Expr t s) s -> ReaderT (Scope s) m (Fun (a t s) s)
ccFun (Fun n e) = Fun n <$> ccExpr e

ccExpr :: (ClosureConvert a, Monad m, Ord s)
       => Expr t s -> ReaderT (Scope s) m (a t s)
ccExpr expr =

    case expr of

        Term t ->
            pure $ term t

        App ty f xs ->
            app ty <$> ccExpr f
                   <*> traverse ccExpr xs

        Lam ty ps b -> do
            Scope scope <- ask
            let isLocal v = case M.lookup v scope of
                                Just Local -> True
                                _          -> False
                env = S.filter isLocal (freeVars b \\ S.fromList ps)
            closure ty env ps <$> binding Local ps (ccExpr b)

        -- The whole chain of let-bound lambdas is in scope in each of them,
        -- and they will all be lifted to toplevel, so are never captured
        Let ty a b@Lam{} c ->
            binding Lifted (map fst . fst $ lamGroup expr) $
                lett ty a <$> ccExpr b
                          <*> ccExpr c

        Let ty a b c ->
            binding Local [a] $
                lett ty a <$> ccExpr b
                          <*> ccExpr c

binding :: (Monad m, Ord s)
        => Binding -> [s] -> ReaderT (Scope s) m r -> ReaderT (Scope s) m r
binding bd ns = local (\(Scope scope) -> Scope (M.fromList [(n, bd) | n <- ns] <> scope))

-- Split off a chain of consecutive let-bound lambdas, as a mutually recursive group
lamGroup :: Expr t s -> ([(s, Expr t s)], Expr t s)
lamGroup = \case

    Let _ a b@Lam{} c ->
        let (group, body) = lamGroup c
        in ((a, b) : group, body)

    e ->
        ([], e)

freeVars :: Ord s => Expr t s -> Set s
freeVars = \case

    Term (Var _ s) ->
        S.singleton s

    Term _ ->
        S.empty

    App _ f xs ->
        S.unions (freeVars f : map freeVars xs)

    Lam _ ps b ->
        freeVars b \\ S.fromList ps

    e@(Let _ _ Lam{} _) ->
        let (group, body) = lamGroup e
        in S.unions (freeVars body : map (freeVars . snd) group)
               \\ S.fromList (map fst group)

    Let _ a b c ->
        S.delete a (freeVars b <> freeVars c)

class (App a, Closure a, CTerm a, Let a)
    => ClosureConvert a

instance ClosureConvert CCExpr

instance App CCExpr where
    app = CCApp

instance Closure CCExpr where
    closure = CCClosure

instance CTerm CCExpr where
    term = CCTerm

instance Let CCExpr where
    lett = CCLet
