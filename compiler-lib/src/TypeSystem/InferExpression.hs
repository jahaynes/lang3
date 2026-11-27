module TypeSystem.InferExpression ( Env
                                  , inferExpr
                                  , seedType
                                  ) where

import Core.Expression
import TypeSystem.Common
import TypeSystem.ConstraintSolver (Constraint (..))
import TypeSystem.Types

import           Control.Monad                    (foldM, mapAndUnzipM, replicateM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (StateT)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict as M
import           Data.String                      (IsString, fromString)
import           Data.String.Interpolate          (iii)

-- | What's in scope: variables, top-level functions and data constructors.
type Env s = Map s (Polytype s)

inferExpr :: (IsString s, Ord s, Show s) => Env s
          -> Expr () s
          -> Infer s ( [Constraint s]
                     , Expr (Type s) s )
inferExpr env expr =

    case expr of

        Term t ->
            (\t' -> ([], Term t')) <$> inferTerm env t

        Lam () vs e -> do
            tvs <- replicateM (length vs) freshTVar
            let env' = M.fromList (zip vs (Forall [] <$> tvs)) `M.union` env
            (cs, e') <- inferExpr env' e
            pure ( cs
                 , Lam (tyArr tvs (typeOf e')) vs e' )

        -- e [] has e's type.
        App () f [] -> do
            (cs, f') <- inferExpr env f
            pure (cs, App (typeOf f') f' [])

        -- The head's arity may not be known yet, so the call is typed by
        -- the solver, once it is (see 'solveGroup'). It's never assumed
        -- from how many arguments the call happens to pass.
        App () f xs -> do
            (c1, f')  <-               inferExpr env  f
            (cs, xs') <- mapAndUnzipM (inferExpr env) xs
            tv        <- freshTVar
            pure ( c1 ++ concat cs ++ [AppC (describeHead f) (typeOf f') (map typeOf xs') tv]
                 , App tv f' xs' )

        -- A let group is recursive: every name in it is in scope in every
        -- right-hand side, and in the body. Let-bound names are monomorphic.
        Let bs body -> do
            seeds <- traverse (\(_, _, e) -> seedType e) bs
            let env' = M.fromList [ (n, Forall [] ty) | ((_, n, _), ty) <- zip bs seeds ]
                           `M.union` env
            (css, bs') <- mapAndUnzipM (\((_, n, e), ty) -> do
                              (cs, e') <- inferExpr env' e
                              pure (Equal ty (typeOf e') : cs, (ty, n, e')))
                          (zip bs seeds)
            (csBody, body') <- inferExpr env' body
            pure ( concat css ++ csBody
                 , Let bs' body' )

        DataCon () c xs -> do
            (cs, xs')        <- mapAndUnzipM (inferExpr env) xs
            ct               <- instantiate =<< lookupCon env c
            (fieldCs, resTy) <- constructorApp c ct (map typeOf xs')
            pure ( concat cs ++ fieldCs
                 , DataCon resTy c xs' )

        Case () scrut alts -> do
            (csScrut, scrut') <- inferExpr env scrut
            tv                <- freshTVar
            (altCs, alts') <- mapAndUnzipM (\(pat, e) -> do
                (csPat, patTy, bindings) <- inferPattern env pat
                (csE, e') <- inferExpr (bindings `M.union` env) e
                pure ( csPat ++ csE ++ [ Equal patTy (typeOf scrut')
                                       , Equal (typeOf e') tv ]
                     , (pat, e') )) alts
            pure ( csScrut ++ concat altCs
                 , Case tv scrut' alts' )

        -- Arithmetic is on Ints and logic on Bools. Equality needs both
        -- operands to have the same type, and gives a Bool.
        BinOp () op a b -> do
            (csA, a') <- inferExpr env a
            (csB, b') <- inferExpr env b
            let (opCs, resTy) = binOpConstraints op (typeOf a') (typeOf b')
            pure ( csA ++ csB ++ opCs
                 , BinOp resTy op a' b' )

-- | The constraints a binary operator puts on its operands' types, and the
-- type of its result.
binOpConstraints :: IsString s => Op -> Type s -> Type s -> ([Constraint s], Type s)
binOpConstraints op ta tb =
    case op of
        Plus   -> both typeInt
        Times  -> both typeInt
        And    -> both typeBool
        Or     -> both typeBool
        Equals -> ([Equal ta tb], typeBool)
    where
    both ty = ([Equal ty ta, Equal ty tb], ty)

inferTerm :: (IsString s, Ord s, Show s) => Env s -> Term () s -> Infer s (Term (Type s) s)
inferTerm env = \case
    Var () x    -> (`Var` x) <$> (instantiate =<< lookupVar env x)
    LitInt n    -> pure (LitInt n)
    LitBool b   -> pure (LitBool b)
    LitString s -> pure (LitString s)

-- Each sub-pattern's own type is threaded up through its parent constructor's
-- instantiated field types, rather than fixed ahead of time, so nested
-- PDataCon patterns get checked against the constructor's real field types.
inferPattern :: (IsString s, Ord s, Show s) => Env s
             -> Pattern s
             -> Infer s ( [Constraint s]
                        , Type s
                        , Env s )
inferPattern _ (PLitInt _) =
    pure ([], typeInt, mempty)

inferPattern _ (PLitBool _) =
    pure ([], typeBool, mempty)

inferPattern _ (PVar v) = do
    tv <- freshTVar
    pure ([], tv, M.singleton v (Forall [] tv))

inferPattern _ PWild = do
    tv <- freshTVar
    pure ([], tv, mempty)

inferPattern env (PDataCon c ps) = do
    subs <- mapM (inferPattern env) ps
    let (css, ts, bindingss) = unzip3 subs
    ct               <- instantiate =<< lookupCon env c
    (fieldCs, resTy) <- constructorApp c ct ts
    bindings         <- mergeBindings bindingss
    pure ( concat css ++ fieldCs
         , resTy
         , bindings )

-- | The variables bound by a constructor pattern's sub-patterns. A
-- variable may only be bound once in a pattern.
mergeBindings :: (IsString s, Ord s, Show s) => [Env s] -> Infer s (Env s)
mergeBindings = foldM step mempty
    where
    step acc m =
        case M.keys (M.intersection acc m) of
            []    -> pure (acc `M.union` m)
            v : _ -> failWith [iii|
                         variable #{renderName v} is bound more than once in a pattern
                     |]

-- | A data constructor (whose instantiated type is given) applied to
-- exactly its fields: constructors aren't first-class, so they can't be
-- partially applied, and must be given every field. Returns the
-- constraints on the fields, and the constructed value's type.
constructorApp :: (IsString s, Show s) => s -> Type s -> [Type s] -> Infer s ([Constraint s], Type s)
constructorApp c ct ts =
    case ct of
        TyArr fields r | length fields == length ts -> pure (zipWith Equal fields ts, r)
        TyArr fields _                              -> wrongCount (length fields)
        r | null ts                                 -> pure ([], r)
          | otherwise                               -> wrongCount 0
    where
    wrongCount n = failWith [iii|
        constructor #{renderName c} expects #{n :: Int} field(s),
        but is given #{length ts}
    |]

-- | The type to give a name before inferring the definition bound to it.
-- A lambda's arity is known from its binders, so its name starts out as an
-- arrow of that arity: a recursive call to it can then be resolved as soon
-- as it's seen, rather than only once the definition has been unified with
-- the name. (Either way gives the same result; this isn't a guess.)
seedType :: (Monad m, IsString s) => Expr t s -> StateT GroupState m (Type s)
seedType = \case
    Lam _ vs@(_:_) _ -> TyArr <$> traverse (const freshTVar) vs <*> freshTVar
    _                -> freshTVar

-- | What a call's head is, for an error message about the call.
describeHead :: Show s => Expr t s -> String
describeHead = \case
    Term (Var _ x) -> renderName x
    Term _         -> "a literal"
    Lam{}          -> "a lambda"
    App{}          -> "the result of a call"
    _              -> "an expression"

lookupVar :: (IsString s, Ord s, Show s) => Env s -> s -> Infer s (Polytype s)
lookupVar env x =
    maybe (failWith [iii|unbound variable: #{renderName x}|]) pure (M.lookup x env)

lookupCon :: (IsString s, Ord s, Show s) => Env s -> s -> Infer s (Polytype s)
lookupCon env c =
    maybe (failWith [iii|unknown constructor: #{renderName c}|]) pure (M.lookup c env)

failWith :: IsString s => String -> Infer s a
failWith = lift . Left . fromString
