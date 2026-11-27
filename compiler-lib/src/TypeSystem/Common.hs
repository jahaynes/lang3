module TypeSystem.Common ( GroupState (..)
                         , Infer
                         , Subst (..)
                         , exprTypes
                         , freshTVar
                         , instantiate
                         , mapExprType
                         , numToVar
                         , substituteType
                         , termType
                         , traverseExprType
                         , typeOf
                         ) where

import Core.Expression  (Expr (..), Term (..))
import TypeSystem.Types

import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.Char                        (chr)
import           Data.Functor.Identity            (Identity (..))
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict as M
import           Data.String                      (IsString, fromString)

newtype GroupState =
    GroupState { getVarNum :: Int
               } deriving Show

type Infer s = StateT GroupState (Either s)

newtype Subst s =
    Subst (Map s (Type s))
        deriving Show

freshTVar :: (Monad m, IsString s) => StateT GroupState m (Type s)
freshTVar = do
    GroupState n <- get
    put (GroupState (n + 1))
    pure . TyVar $ numToVar n

numToVar :: IsString s => Int -> s
numToVar n =
    let (num, letter) = n `divMod` 26
    in fromString (chr (letter + 97) : show num)

substituteType :: Ord s => Subst s -> Type s -> Type s
substituteType         _       t@TyCon{} = t
substituteType (Subst s)     t@(TyVar a) = M.findWithDefault t a s
substituteType         s (TyArr ps r)    = TyArr (map (substituteType s) ps) (substituteType s r)
substituteType         s (t1 `TyApp` t2) = substituteType s t1 `TyApp` substituteType s t2

instantiate :: (Monad m, Ord s, IsString s) => Polytype s -> StateT GroupState m (Type s)
instantiate (Forall as t) = do
    as' <- mapM (const freshTVar) as
    let s = Subst $ M.fromList $ zip as as'
    pure $ substituteType s t

typeOf :: IsString s => Expr (Type s) s -> Type s
typeOf = \case
    Term t        -> termType t
    App t _ _     -> t
    Lam t _ _     -> t
    Let _ body    -> typeOf body
    DataCon t _ _ -> t
    Case t _ _    -> t
    BinOp t _ _ _ -> t

termType :: IsString s => Term (Type s) s -> Type s
termType = \case
    Var t _     -> t
    LitInt _    -> typeInt
    LitBool _   -> typeBool
    LitString _ -> typeString

mapExprType :: (t -> u) -> Expr t s -> Expr u s
mapExprType f = runIdentity . traverseExprType (Identity . f)

traverseExprType :: Applicative f => (t -> f u) -> Expr t s -> f (Expr u s)
traverseExprType f = go
    where
    go = \case
        Term t            -> Term <$> goTerm t
        App t g xs        -> App <$> f t <*> go g <*> traverse go xs
        Lam t vs body     -> Lam <$> f t <*> pure vs <*> go body
        Let bs body       -> Let <$> traverse (\(t, n, e) -> (,,) <$> f t <*> pure n <*> go e) bs
                                 <*> go body
        DataCon t c xs    -> DataCon <$> f t <*> pure c <*> traverse go xs
        Case t scrut alts -> Case <$> f t <*> go scrut <*> traverse (\(p, e) -> (,) p <$> go e) alts
        BinOp t op a b    -> BinOp <$> f t <*> pure op <*> go a <*> go b

    goTerm = \case
        Var t x     -> (`Var` x) <$> f t
        LitInt n    -> pure (LitInt n)
        LitBool b   -> pure (LitBool b)
        LitString s -> pure (LitString s)

exprTypes :: Expr t s -> [t]
exprTypes = \case
    Term (Var t _)    -> [t]
    Term _            -> []
    App t f xs        -> t : concatMap exprTypes (f : xs)
    Lam t _ body      -> t : exprTypes body
    Let bs body       -> concat [ t : exprTypes e | (t, _, e) <- bs ] ++ exprTypes body
    DataCon t _ xs    -> t : concatMap exprTypes xs
    Case t scrut alts -> t : exprTypes scrut ++ concatMap (exprTypes . snd) alts
    BinOp t _ a b     -> t : exprTypes a ++ exprTypes b
