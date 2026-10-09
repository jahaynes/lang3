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
        deriving (Eq, Show)

newtype Scope s
    = Scope (Map s Binding)

data CCExpr t s
    = CCTerm !(Term t s)
    | CCApp !t !(CCExpr t s) ![CCExpr t s]
    | CCClosure !t !(Set s) ![s] !(CCExpr t s)
    | CCLet ![(t, s, CCExpr t s)] !(CCExpr t s)
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
            scope <- ask
            let env = captures scope (freeVars expr)
            closure ty env ps <$> binding Local ps (ccExpr b)

        -- A group of lambdas which capture no locals can be lifted to toplevel,
        -- so references to them are never captured. Anything else is a local value.
        Let bs body -> do
            scope <- ask
            let names   = [n | (_, n, _) <- bs]
                rhsFree = S.unions [freeVars e | (_, _, e) <- bs] \\ S.fromList names
                bd | all (\(_, _, e) -> isLam e) bs
                   , S.null (captures scope rhsFree) = Lifted
                   | otherwise                       = Local
            binding bd names $
                lett <$> traverse (\(t, n, e) -> (\e' -> (t, n, e')) <$> ccExpr e) bs
                     <*> ccExpr body

binding :: (Monad m, Ord s)
        => Binding -> [s] -> ReaderT (Scope s) m r -> ReaderT (Scope s) m r
binding bd ns = local (\(Scope scope) -> Scope (M.fromList [(n, bd) | n <- ns] <> scope))

-- The free variables which must be stored in a closure's environment
captures :: Ord s => Scope s -> Set s -> Set s
captures (Scope scope) = S.filter (\v -> M.lookup v scope == Just Local)

isLam :: Expr t s -> Bool
isLam Lam{} = True
isLam _     = False

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

    Let bs body ->
        S.unions (freeVars body : [freeVars e | (_, _, e) <- bs])
            \\ S.fromList [n | (_, n, _) <- bs]

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
