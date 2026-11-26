module Transform.ClosureConvert ( CCExpr (..)
                                , closureConvert
                                ) where

import Core.Expression (Expr (..), Pattern, Term (..), patternVars)
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
    | CCDataCon !t !s ![CCExpr t s]
    | CCCase !t !(CCExpr t s) ![(Pattern s, CCExpr t s)]
        deriving (Eq, Show)

closureConvert :: (ClosureConvert a, Monad m, Ord s)
               => Module (Expr t s) s
               -> m (Module (a t s) s)
closureConvert md = do

    let funDefs  = getFunDefns md
        funNames = map (\(Fun n _) -> n) funDefs
        scope    = Scope (M.fromList (zip funNames (repeat Global)))

    funDefs' <- runReaderT (traverse ccFun funDefs) scope

    pure Module { getFunDefns = funDefs' }

ccFun :: (ClosureConvert a, Monad m, Ord s)
      => Fun (Expr t s) s
      -> ReaderT (Scope s) m (Fun (a t s) s)
ccFun (Fun n e) = Fun n <$> ccExpr (annotate e)

ccExpr :: (ClosureConvert a, Monad m, Ord s)
       => FVExpr t s
       -> ReaderT (Scope s) m (a t s)
ccExpr (FVExpr fvs expr) =

    case expr of

        FVTerm t ->
            pure $ term t

        FVApp ty f xs ->
            app ty <$> ccExpr f
                   <*> traverse ccExpr xs

        FVLam ty ps b -> do
            scope <- ask
            let env = captures scope fvs
            closure ty env ps <$> binding Local ps (ccExpr b)

        FVLet bs body -> do
            scope <- ask
            let names   = map bindName bs
                rhsFree = foldMap (freeVars . bindExpr) bs \\ S.fromList names
                bd | all (isLam . bindExpr) bs
                   , S.null (captures scope rhsFree) = Lifted
                   | otherwise                       = Local
            binding bd names $
                lett <$> traverse (\(t, n, e) -> (\e' -> (t, n, e')) <$> ccExpr e) bs
                     <*> ccExpr body

        FVDataCon ty c xs ->
            dataCon ty c <$> traverse ccExpr xs

        FVCase ty scrut alts ->
            caseOf ty <$> ccExpr scrut
                      <*> traverse ccAlt alts

    where
    ccAlt (pat, body) =
        (,) pat <$> binding Local (patternVars pat) (ccExpr body)

    isLam :: FVExpr t s -> Bool
    isLam (FVExpr _ FVLam{}) = True
    isLam _                  = False

    captures :: Ord s => Scope s -> Set s -> Set s
    captures (Scope scope) = S.filter (\v -> M.lookup v scope == Just Local)

binding :: (Monad m, Ord s)
        => Binding
        -> [s]
        -> ReaderT (Scope s) m r
        -> ReaderT (Scope s) m r
binding bd ns = local f
    where
    f (Scope scope) = Scope (foldr (\n -> M.insert n bd) scope ns)

bindName :: (t, s, e) -> s
bindName (_, n, _) = n

bindExpr :: (t, s, e) -> e
bindExpr (_, _, e) = e

mapBindExpr :: (e -> e') -> (t, s, e) -> (t, s, e')
mapBindExpr f (t, n, e) = (t, n, f e)

data FVExpr t s
    = FVExpr { freeVars :: !(Set s)
             , _fvNode  :: !(FVNode t s)
             }

data FVNode t s
    = FVTerm !(Term t s)
    | FVApp !t !(FVExpr t s) ![FVExpr t s]
    | FVLam !t ![s] !(FVExpr t s)
    | FVLet ![(t, s, FVExpr t s)] !(FVExpr t s)
    | FVDataCon !t !s ![FVExpr t s]
    | FVCase !t !(FVExpr t s) ![(Pattern s, FVExpr t s)]

annotate :: Ord s => Expr t s -> FVExpr t s
annotate = \case

    Term t ->
        FVExpr (termVars t) (FVTerm t)

    App ty f xs ->
        let f'  = annotate f
            xs' = map annotate xs
        in FVExpr (S.unions (freeVars f' : map freeVars xs'))
                  (FVApp ty f' xs')

    Lam ty ps b ->
        let b' = annotate b
        in FVExpr (freeVars b' \\ S.fromList ps)
                  (FVLam ty ps b')

    Let bs body ->
        let bs'   = map (mapBindExpr annotate) bs
            body' = annotate body
        in FVExpr ((freeVars body' <> foldMap (freeVars . bindExpr) bs')
                       \\ S.fromList (map bindName bs'))
                  (FVLet bs' body')

    DataCon ty c xs ->
        let xs' = map annotate xs
        in FVExpr (S.unions (map freeVars xs'))
                  (FVDataCon ty c xs')

    Case ty scrut alts ->
        let scrut' = annotate scrut
            alts'  = map (\(p, b) -> (p, annotate b)) alts
        in FVExpr (S.unions (freeVars scrut'
                                : [ freeVars b \\ S.fromList (patternVars p)
                                  | (p, b) <- alts' ]))
                  (FVCase ty scrut' alts')

    where
    termVars (Var _ s) = S.singleton s
    termVars _         = S.empty

class (App a, Case a, Closure a, CTerm a, DataCon a, Let a)
    => ClosureConvert a

instance ClosureConvert CCExpr

instance App CCExpr where
    app = CCApp

instance Case CCExpr where
    caseOf = CCCase

instance Closure CCExpr where
    closure = CCClosure

instance CTerm CCExpr where
    term = CCTerm

instance DataCon CCExpr where
    dataCon = CCDataCon

instance Let CCExpr where
    lett = CCLet
