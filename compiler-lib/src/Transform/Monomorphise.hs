module Transform.Monomorphise ( monomorphise
                              ) where

import Core.Expression             (Expr (..), Term (..), patternVars)
import Core.Module                 (Fun (..), Module (..))
import TypeSystem.CallGraph        (CallGraph (..))
import TypeSystem.Common           (Subst (..), exprTypes, mapExprType, substituteType,
                                    traverseExprType, typeOf)
import TypeSystem.ConstraintSolver (freeInType)
import TypeSystem.Types            (MType (..), Type (..), renderName, renderType, typeInt)
import TypeSystem.TypesCallGraph   (buildModuleGraph)

import           Control.Monad                (foldM)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.RWS.CPS  (RWST, asks, evalRWST, get, put, tell)
import           Data.Foldable                (toList)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict as M
import           Data.Sequence                (Seq)
import qualified Data.Sequence as Seq
import           Data.Set                     (Set)
import qualified Data.Set as S
import           Data.String                  (IsString, fromString)
import           Data.String.Interpolate      (iii)

-- | Replace every generic top-level function with one copy per concrete
-- instantiation it is used at, so that no type variable is left anywhere.
--
-- A top-level function's quantified variables are all the type variables
-- in its annotations (see 'TypeSystem.InferModule.generalise').
--
-- Roots: every monomorphic function, and every function nothing else
-- calls (e.g. "main"), are kept under their own name. A generic root has
-- its quantified variables defaulted (see 'totalise'). Everything else is
-- emitted only as the specialisations reachable from the roots, named
-- @f$T@ for @f@ at concrete type @T@ -- "$" can't occur in a source
-- identifier, so these can't clash with the program's own names.
monomorphise :: (IsString s, Ord s, Show s)
             => Module (Expr (Type s) s) s
             -> Either s (Module (Expr (MType s) s) s)
monomorphise md = do

    let funs   = getFunDefns md

        funMap = M.fromList [ (n, (quantified e, e)) | Fun n e <- funs ]

        called = let CallGraph cg = buildModuleGraph md
                 in S.unions (M.elems cg)

        initial = [ f | f@(Fun n e) <- funs
                      , S.null (quantified e) || n `S.notMember` called ]

    (emitted, extras) <- evalRWST (traverse emitUnderOwnName initial) funMap mempty

    pure md { getFunDefns = emitted ++ toList extras }

    where
    emitUnderOwnName (Fun n e) = do
        let subst       = totalise (quantified e) mempty
            substituted = mapExprType (substituteType (Subst subst)) e
        e' <- specExpr substituted
        Fun n <$> lift (monoExpr e')

quantified :: Ord s => Expr (Type s) s -> Set s
quantified = foldMap freeInType . exprTypes

monoExpr :: (IsString s, Show s) => Expr (Type s) s -> Either s (Expr (MType s) s)
monoExpr = traverseExprType monoType

monoType :: (IsString s, Show s) => Type s -> Either s (MType s)
monoType (TyVar v)    = Left $ fromString
                            [iii|Monomorphise: unresolved type variable #{renderName v}|]
monoType (TyCon c)    = pure (MTyCon c)
monoType (TyArr ps r) = MTyArr <$> traverse monoType ps <*> monoType r
monoType (TyApp f x)  = MTyApp <$> monoType f <*> monoType x

type Mono s = RWST -- Reader: the module's top-level functions by name, with
                   -- their quantified type variables.
                   (Map s (Set s, Expr (Type s) s))

                   -- Writer: specialised copies, in the order they finish being built.
                   (Seq (Fun (Expr (MType s) s) s))

                   -- State: memo of (generic name, concrete instantiation) -> specialised name.
                   (Map (s, Type s) s)

                   (Either s)

-- | Specialise every reference to a generic top level. A lambda-, let- or
-- pattern-bound name hides a top level of the same name, so is left alone.
specExpr :: (IsString s, Ord s, Show s) => Expr (Type s) s -> Mono s (Expr (Type s) s)
specExpr = go S.empty

    where
    go bound = \case

        Term (Var t x) | x `S.notMember` bound ->

            asks (M.lookup x) >>= \case

                Just (qs, body) | not (S.null qs) ->
                    Term . Var t <$> specialise x qs body t

                _ ->
                    pure (Term (Var t x))

        Term tm ->
            pure (Term tm)

        App t f xs ->
            App t <$> go bound f <*> traverse (go bound) xs

        Lam t vs body ->
            Lam t vs <$> go (bound <> S.fromList vs) body

        Let bs body ->
            let bound' = bound <> S.fromList [ n | (_, n, _) <- bs ]
            in Let <$> traverse (\(t, n, e) -> (,,) t n <$> go bound' e) bs
                   <*> go bound' body

        DataCon t c xs ->
            DataCon t c <$> traverse (go bound) xs

        Case t scrut alts ->
            Case t <$> go bound scrut <*> traverse alt alts
            where
            alt (p, e) = (,) p <$> go (bound <> S.fromList (patternVars p)) e

        BinOp t op a b ->
            BinOp t op <$> go bound a <*> go bound b

-- | Get-or-create the specialisation of a generic function's body at one
-- concrete instantiation. The memo entry is recorded before recursing into
-- the specialised body, so a generic that calls itself (or a function in
-- its own binding group) at the same instantiation resolves back to the
-- copy already being built, instead of looping forever.
specialise :: (IsString s, Ord s, Show s) => s
                                          -> Set s
                                          -> Expr (Type s) s
                                          -> Type s
                                          -> Mono s s
specialise origName qs body concreteTy = do

    memo <- get

    case M.lookup (origName, concreteTy) memo of

        Just specName ->
            pure specName

        Nothing -> do
            matched <- lift (matchInstantiation qs (typeOf body) concreteTy)

            let subst    = totalise qs matched
                specName = fromString (renderName origName ++ "$" ++ renderType concreteTy)

            put $! M.insert (origName, concreteTy) specName memo

            let substituted = mapExprType (substituteType (Subst subst)) body
            specialisedBody <- specExpr substituted

            mono <- lift (monoExpr specialisedBody)
            tell (Seq.singleton (Fun specName mono))

            pure specName

-- | Match a generalised function's own type against one concrete
-- instantiation of it, recovering the substitution from each quantified
-- variable to the concrete type it stands for at that use.
matchInstantiation :: (IsString s, Ord s, Show s) => Set s
                                                  -> Type s
                                                  -> Type s
                                                  -> Either s (Map s (Type s))
matchInstantiation qs generalised concrete = go generalised concrete

    where
    go (TyVar v) ct
        | v `S.member` qs = pure (M.singleton v ct)
        | otherwise       = pure mempty

    go (TyCon c) (TyCon c')
        | c == c' = pure mempty

    go (TyApp f x) (TyApp f' x') = do
        a <- go f f'
        b <- go x x'
        merge a b

    go (TyArr ps r) (TyArr ps' r')
        | length ps == length ps' =
            foldM merge mempty =<< sequence (go r r' : zipWith go ps ps')

    go _ _ =
        Left $ fromString [iii|
            Monomorphise: can't match generalised type #{renderType generalised}
            against concrete instantiation #{renderType concrete}
        |]

    merge a b
        | and (M.intersectionWith (==) a b) = pure (a <> b)
        | otherwise = Left $ fromString [iii|
            Monomorphise: inconsistent instantiation of #{renderType generalised}
            at #{renderType concrete}
        |]

-- | Bind whatever 'matchInstantiation' left out: a quantified variable that
-- doesn't occur in the function's own type, so no use of the function can
-- pin it down (or, for a root, every quantified variable). Every such
-- variable defaults to the same placeholder -- which one doesn't matter,
-- since nothing ever observes a value of that type.
totalise :: (IsString s, Ord s) => Set s -> Map s (Type s) -> Map s (Type s)
totalise qs matched =
    matched <> M.fromSet (const typeInt) (qs S.\\ M.keysSet matched)
