module Transform.EtaSaturate ( etaSaturate
                             ) where

import Core.Expression  (Expr (..), Term (..), patternVars)
import Core.Module      (Fun (..), Module (..))
import TypeSystem.Common (mTypeOf)
import TypeSystem.Types  (MType (..), mArity, mTyArr)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Reader       (ReaderT, asks, local, runReaderT)
import           Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import           Data.Set                         (Set)
import qualified Data.Set as S
import           Data.String                      (IsString, fromString)
import           Data.String.Interpolate          (iii)

-- Saturates calls, since the runtime has no partial-application objects:
-- every call must pass exactly as many arguments as its callee takes.
--
-- Arity is part of a function's type (@(Int, Int) -> Int@ takes two
-- arguments at once; @Int -> Int -> Int@ takes one, and returns a function
-- taking one), and inference has already checked every call against it: no
-- call passes more arguments than its head takes. So each call is
-- saturated just by looking at its head's type:
--
--   * a call with all its head's arguments is left alone;
--
--   * a call short of some becomes a lambda over the rest:
--
--         map (plus 1)   ~>   map (\eta$0 -> plus 1 eta$0)
--
--     Evaluation is strict, so a head or argument that isn't already a
--     value is let-bound outside the new lambda, in evaluation order, so
--     that it's still evaluated once and in the same place:
--
--         f (g y)   ~>   let eta$0 = g y in \eta$1 -> f eta$0 eta$1
--
--   * a partial call that is itself called, (f x) y, is one call, f x y.
--
-- The one function whose arity isn't its type's is a top level that isn't
-- a lambda (inc = plus 1): it's compiled as a 0-parameter function (see
-- 'Transform.Hoist.hFun'), so a reference to it is a call to it, and inc 41
-- calls it, then its result. Such a top level is never itself made into a
-- lambda: if it saturates to one, it returns that lambda instead.
--
-- \ -> e and e [] both have e's type, so both mean e: \ -> e becomes e,
-- even bound by a let (which is by value), and e [] becomes e unless e is a
-- 0-parameter top level, which it then calls. Only a top level \ -> e is
-- kept, as a 0-parameter function like any other top level that isn't a
-- lambda.
--
-- Nothing here fails on a well-typed program: the one error is a call
-- inference should already have rejected.
etaSaturate :: (IsString s, Ord s, Show s)
            => Module (Expr (MType s) s) s
            -> Either s (Module (Expr (MType s) s) s)
etaSaturate md = do

    fs' <- evalStateT (runReaderT (traverse eFun fs) nullaryTops) 0

    pure md { getFunDefns = fs' }

    where
    fs = getFunDefns md

    nullaryTops = S.fromList [ n | Fun n body <- fs, not (takesArguments body) ]

    takesArguments = \case
        Lam _ (_:_) _ -> True
        _             -> False

-- | Reads the 0-parameter top levels in scope, and threads the next
-- fresh-name counter.
type Sat s = ReaderT (Set s) (StateT Int (Either s))

eFun :: (IsString s, Ord s, Show s) => Fun (Expr (MType s) s) s -> Sat s (Fun (Expr (MType s) s) s)
eFun (Fun n body) =
    Fun n <$> case body of
                  -- Keeps its binders (none, for \ -> e: still a
                  -- 0-parameter function).
                  Lam t vs b ->
                      Lam t vs <$> local (hiding vs) (saturate b)
                  _ -> do
                      body' <- saturate body
                      case body' of
                          -- Returns the lambda, rather than becoming it.
                          Lam t _ _ -> do
                              x <- fresh
                              pure (Let [(t, x, body')] (Term (Var t x)))
                          _ ->
                              pure body'

-- | A lambda-, let- or pattern-bound name hides any top level of that name.
hiding :: Ord s => [s] -> Set s -> Set s
hiding vs nullary = nullary `S.difference` S.fromList vs

-- | A name not handed out before. "$" can't occur in a source identifier,
-- so it can't capture or be captured by anything the program names.
fresh :: IsString s => Sat s s
fresh = do
    n <- lift get
    lift (put (n + 1))
    pure (fromString ("eta$" ++ show n))

saturate :: (IsString s, Ord s, Show s) => Expr (MType s) s -> Sat s (Expr (MType s) s)
saturate expr =

    case expr of

        Term Var{} ->
            call expr []

        Term _ ->
            pure expr

        App _ f xs -> do
            (hd, args) <- spine f xs
            hd'   <- case hd of
                         Term Var{} -> pure hd
                         _          -> saturate hd
            args' <- traverse saturate args
            call hd' args'

        -- \ -> b has b's type, so as a value it is b. (As a top level, a
        -- 0-parameter function, it's kept by 'eFun'.)
        Lam _ [] b ->
            saturate b

        Lam t vs b ->
            Lam t vs <$> local (hiding vs) (saturate b)

        -- A let group is recursive: its names are in scope in every
        -- right-hand side and in the body. A let is by value, so a let of
        -- \ -> e is a let of e.
        Let bs body -> do
            let ns = [ n | (_, n, _) <- bs ]
            local (hiding ns) $
                Let <$> traverse (\(t, n, e) -> (,,) t n <$> saturate (unNullary e)) bs
                    <*> saturate body

        DataCon t c xs ->
            DataCon t c <$> traverse saturate xs

        Case t scrut alts -> do
            let alt (p, b) = (,) p <$> local (hiding (patternVars p)) (saturate b)
            Case t <$> saturate scrut <*> traverse alt alts

        BinOp t op a b ->
            BinOp t op <$> saturate a <*> saturate b

-- | A call's head, and its arguments: a partial call that is itself called,
-- (f x) y, is one call, f x y. A call that already has all its head's
-- arguments isn't merged with one of its result.
spine :: (IsString s, Ord s) => Expr (MType s) s
                             -> [Expr (MType s) s]
                             -> Sat s (Expr (MType s) s, [Expr (MType s) s])
spine f xs =
    case f of
        App _ g ys -> do
            m <- calleeArity g
            if length ys < m
                then spine g (ys ++ xs)
                else pure (f, xs)
        _ ->
            pure (f, xs)

-- | How many arguments a head takes: its type's arity, except for a
-- 0-parameter top level.
calleeArity :: (IsString s, Ord s) => Expr (MType s) s -> Sat s Int
calleeArity = \case
    Term (Var t x) -> do
        nullary <- asks (S.member x)
        pure (if nullary then 0 else mArity t)
    e ->
        pure (mArity (mTypeOf e))

-- | Call an (already saturated) head with (already saturated) arguments,
-- passing it exactly as many as it takes.
call :: (IsString s, Ord s, Show s) => Expr (MType s) s -> [Expr (MType s) s] -> Sat s (Expr (MType s) s)
call hd args = do
    nullaryHead <- case hd of
                       Term (Var _ x) -> asks (S.member x)
                       _              -> pure False
    case () of
        -- A 0-parameter top level is called on reference, and its result
        -- then called with the arguments.
        _ | nullaryHead ->
            let called = App (mTypeOf hd) hd []
            in if null args then pure called else call called args

        -- e [] is e.
        _ | null args ->
            pure hd

        _ -> case mTypeOf hd of
            MTyArr ps r
                | n == m -> pure (App r hd args)
                | n <  m -> do
                    (hdBinds, hd')    <- share hd
                    (argBinds, args') <- unzip <$> traverse share args
                    ys                <- traverse (const fresh) missing
                    let body  = App r hd' (args' ++ zipWith (\t y -> Term (Var t y)) missing ys)
                        lam   = Lam (mTyArr missing r) ys body
                        binds = hdBinds ++ concat argBinds
                    pure (foldr bind lam binds)
                where n       = length args
                      m       = length ps
                      missing = drop n ps
            ty -> lift . lift . Left $ fromString [iii|
                      eta: a call passes #{length args} argument(s) to a head of type
                      #{show ty}, which takes fewer (inference should have rejected this)
                  |]

    where
    -- Let-bind an expression that isn't a value, so it can be moved under a
    -- lambda without changing when or how often it's evaluated.
    share e
        | isValue e = pure ([], e)
        | otherwise = do
              a <- fresh
              pure ([(a, e)], Term (Var (mTypeOf e) a))

    isValue = \case
        Term{} -> True
        Lam{}  -> True
        _      -> False

    bind (a, rhs) body = Let [(mTypeOf rhs, a, rhs)] body

-- | \ -> e, as a value, is e.
unNullary :: Expr t s -> Expr t s
unNullary = \case
    Lam _ [] b -> unNullary b
    e          -> e

