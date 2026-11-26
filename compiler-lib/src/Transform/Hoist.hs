module Transform.Hoist ( HExpr (..)
                       , HFun (..)
                       , HModule (..)
                       , Hoist
                       , hoist
                       ) where

import Core.Expression          (Pattern, Term (..), patternVars)
import Core.Module              (Fun (..), Module (..))
import Transform.Class
import Transform.ClosureConvert (CCExpr (..))

import           Control.Monad.Trans.RWS.Strict (RWST, ask, get, local, put, runRWST, tell)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict as M
import           Data.Set                       (Set)
import qualified Data.Set as S
import           Data.String                    (IsString (..))

type H t s m a =
    RWST (Map s s) [HFun t s] Int m a

hoist :: (Monad m, Ord s, IsString s)
      => Module (CCExpr t s) s
      -> m (HModule t s)
hoist md = do
    (fs', _, funs) <- runRWST (traverse hFun (getFunDefns md))
                              M.empty
                              0
    pure $ HModule (funs <> fs')

hFun :: (Monad m, Ord s, IsString s)
     => Fun (CCExpr t s) s
     -> H t s m (HFun t s)
hFun (Fun name e) =

    case e of

        -- Top level function
        CCClosure _ty env ps body ->
            HFun name env ps <$> shadow ps (hExpr body)

        _nonFunction ->
            HFun name mempty [] <$> hExpr e

hExpr :: (Hoist a, Monad m, Ord s, IsString s)
      => CCExpr t s
      -> H t s m (a t s)
hExpr e =

    case e of

        CCTerm (Var ty v) -> do
            v' <- M.findWithDefault v v <$> ask
            pure $ term (Var ty v')

        CCTerm t ->
            pure $ term t

        CCApp ty f xs ->
            app ty <$> hExpr f
                   <*> traverse hExpr xs

        CCClosure ty env ps body -> do
            k <- fresh
            emit k env ps body
            pure $ mkClo ty env k

        CCLet bs body

            | Just lams <- traverse liftable bs -> do
                let names = map (\(n, _, _, _) -> n) lams
                names' <- traverse (const fresh) names
                withRenames (zip names names') $ do
                    mapM_ (\(k, (_, env, ps, b)) -> emit k env ps b)
                          (zip names' lams)
                    hExpr body

            | otherwise ->
                shadow (map (\(_, n, _) -> n) bs) $
                    lett <$> traverse (\(t, n, x) -> (\x' -> (t, n, x')) <$> hExpr x) bs
                         <*> hExpr body

        CCDataCon ty c xs ->
            dataCon ty c <$> traverse hExpr xs

        CCCase ty scrut alts ->
            caseOf ty <$> hExpr scrut
                      <*> traverse hAlt alts

    where
    hAlt (pat, body) =
        (,) pat <$> shadow (patternVars pat) (hExpr body)

    liftable (_, n, CCClosure _ env ps b) | S.null env = Just (n, env, ps, b)
    liftable _                                         = Nothing

emit :: (Monad m, Ord s, IsString s)
     => s
     -> Set s
     -> [s]
     -> CCExpr t s
     -> H t s m ()
emit k env ps body = do
    body' <- shadow (S.toList env <> ps) (hExpr body)
    tell [HFun k env ps body']

fresh :: (Monad m, IsString s) => H t s m s
fresh = do
    n <- get
    put (n + 1)
    pure . fromString $ "ll_" <> show n

shadow :: (Monad m, Ord s) => [s] -> H t s m a -> H t s m a
shadow ns = local $ \rn -> foldr M.delete rn ns

withRenames :: (Monad m, Ord s) => [(s, s)] -> H t s m a -> H t s m a
withRenames rs = local $ M.union (M.fromList rs)

newtype HModule t s =
    HModule [HFun t s]
        deriving Show

data HFun t s =
    HFun !s !(Set s) ![s] !(HExpr t s)
        deriving (Eq, Show)

data HExpr t s
    = HTerm !(Term t s)
    | HApp !t !(HExpr t s) ![HExpr t s]
    | HMkClo !t !(Set s) !s
    | HLet ![(t, s, HExpr t s)] !(HExpr t s)
    | HDataCon !t !s ![HExpr t s]
    | HCase !t !(HExpr t s) ![(Pattern s, HExpr t s)]
        deriving (Eq, Show)

class (App a, Case a, CTerm a, DataCon a, Let a, MkClo a)
    => Hoist a

instance Hoist HExpr

instance App HExpr where
    app = HApp

instance Case HExpr where
    caseOf = HCase

instance CTerm HExpr where
    term = HTerm

instance DataCon HExpr where
    dataCon = HDataCon

instance Let HExpr where
    lett = HLet

instance MkClo HExpr where
    mkClo = HMkClo
