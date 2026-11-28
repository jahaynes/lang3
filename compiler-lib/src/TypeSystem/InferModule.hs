module TypeSystem.InferModule ( inferModule
                              ) where

import Core.Expression             (Expr)
import Core.Module                 (Fun (..), Module (..))
import TypeSystem.CallGraph        (createPlan)
import TypeSystem.Common
import TypeSystem.ConstraintSolver
import TypeSystem.InferExpression
import TypeSystem.Types
import TypeSystem.TypesCallGraph   (buildModuleGraph)

import           Control.Monad                    (foldM, mapAndUnzipM)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.List                        (group, sort)
import           Data.Map.Strict                  (Map)
import qualified Data.Map.Strict as M
import           Data.Set                         (Set)
import qualified Data.Set as S
import           Data.String                      (IsString, fromString)
import           Data.String.Interpolate          (iii)

-- | Infer a type for every top-level function, and annotate every node of
-- its body with its type. Top-level functions are generalised: the type
-- variables left in a function's annotations once its binding group is
-- solved are renamed a0, b0, ... and quantified over. The output keeps the
-- input's function order.
--
-- Functions are inferred in dependency order, one binding group (a set of
-- mutually recursive functions) at a time: see 'createPlan'.
inferModule :: (IsString s, Ord s, Show s)
            => Module (Expr () s) s
            -> Either s (Module (Expr (Type s) s) s)
inferModule md = do

    let dataDefns = getDataDefns md
        funDefns  = getFunDefns md
        untyped  = M.fromList [ (n, e) | Fun n e <- funDefns ]

    checkDuplicates $ [ n | Fun n _ <- funDefns ]
                   <> [ c | DataDefn _ _ cs <- dataDefns, DCon c _ <- cs ]

    ctorEnv <- foldM dataDefnToType mempty dataDefns

    let plan = createPlan (buildModuleGraph md)

        step (env, done) grp = do
            (envDelta, typed) <- inferGroup env [ (n, untyped M.! n) | n <- S.toList grp ]
            pure (envDelta `M.union` env, M.fromList typed `M.union` done)

    (_, typed) <- evalStateT (foldM step (ctorEnv, mempty) plan) (GroupState 0)

    pure md { getFunDefns = [ Fun n (typed M.! n) | Fun n _ <- funDefns ] }

-- | Top-level functions and data constructors share one namespace.
checkDuplicates :: (IsString s, Ord s, Show s) => [s] -> Either s ()
checkDuplicates names =
    case [ n | n : _ : _ <- group (sort names) ] of
        []    -> Right ()
        n : _ -> Left $ fromString [iii|duplicate definition of #{renderName n}|]

-- Every constructor of a type is quantified over exactly that type's own
-- declared parameters, and its result type is the type constructor applied
-- to those same parameters -- so e.g. Cons :: forall a. a -> List a -> List
-- a, and Nil :: forall a. List a, even though "a" doesn't occur in Nil's
-- (empty) field list. This is what makes List's occurrences agree in arity
-- everywhere: without a declared parameter list, a nullary constructor like
-- Nil has no field to read a type parameter off, and comes out as the
-- monotype `List` (zero type arguments) -- a different, unifiable-only-with-
-- itself application of the same type constructor than `List a`.
dataDefnToType :: (IsString s, Ord s, Show s) => Env s
                                              -> DataDefn s
                                              -> Either s (Env s)
dataDefnToType env (DataDefn typeName params dcons) =

    (`M.union` env) . M.fromList <$> traverse go dcons

    where
    declared = S.fromList params
    resultTy = foldl TyApp (TyCon typeName) (map TyVar params)

    go (DCon dc fields) =
        let escaping = foldMap freeInType fields S.\\ declared
        in if S.null escaping
            then Right (dc, Forall params (tyArr fields resultTy))
            else Left $ fromString [iii|
                     undeclared type variable(s) #{unwords (map renderName (S.toList escaping))}
                     in constructor #{renderName dc} of data type #{renderName typeName}
                 |]

inferGroup :: (IsString s, Ord s, Show s) => Env s
                                          -> [(s, Expr () s)]
                                          -> Infer s (Env s, [(s, Expr (Type s) s)])
inferGroup prevEnv grp = do

    -- Assign fresh metavariables to all function names in this group (an
    -- arrow of them, for a lambda: see 'seedType')
    seeds <- traverse (seedType . snd) grp

    -- Within the group, a function's name has the type of its definition
    -- (recursion is monomorphic).
    let env = M.fromList (zip (map fst grp) (Forall [] <$> seeds)) `M.union` prevEnv

    (css, typed) <- mapAndUnzipM (\((n, e), seed) -> do
                        (cs, e') <- inferExpr env e
                        pure (Equal seed (typeOf e') : cs, (n, e')))
                    (zip grp seeds)

    solved <- lift (solveGroup (concat css))

    -- Apply the mapping of metavariables to monotypes, and generalise
    let generalised = [ (n, generalise solved e') | (n, e') <- typed ]

        envDelta = M.fromList [ (n, Forall (S.toList (typeVars e')) (typeOf e'))
                              | (n, e') <- generalised ]

    pure (envDelta, generalised)

-- | Substitute the solution into an expression's annotations, then rename
-- the type variables left over to a0, b0, ...
generalise :: (IsString s, Ord s) => Subst s -> Expr (Type s) s -> Expr (Type s) s
generalise subst tex =

    mapExprType (rename sub) tex'

    where
    tex' = mapExprType (substituteType subst) tex
    sub  = M.fromList $ zip (S.toList (typeVars tex')) (map numToVar [0..])

    rename :: Ord s => Map s s -> Type s -> Type s
    rename _ t@TyCon{}   = t
    rename m (TyVar v)   = TyVar (M.findWithDefault v v m)
    rename m (TyArr ps r) = TyArr (map (rename m) ps) (rename m r)
    rename m (TyApp f x) = TyApp (rename m f) (rename m x)

typeVars :: Ord s => Expr (Type s) s -> Set s
typeVars = foldMap freeInType . exprTypes
