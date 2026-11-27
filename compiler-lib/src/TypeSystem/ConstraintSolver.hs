module TypeSystem.ConstraintSolver ( Constraint (..)
                                   , compose
                                   , freeInType
                                   , solveGroup
                                   , unifies
                                   ) where

import TypeSystem.Common (Subst (..), substituteType)
import TypeSystem.Types

import qualified Data.Map as M
import           Data.Set                (Set)
import qualified Data.Set as S
import           Data.String             (IsString, fromString)
import           Data.String.Interpolate (iii)

data Constraint s
    = Equal (Type s) (Type s)
      -- | A call whose head's arity may not be known yet: a description of
      -- the head (for errors), the head's type, the arguments' types, and
      -- the call's own type. See 'solveGroup'.
    | AppC String (Type s) [Type s] (Type s)
        deriving (Eq, Show)

-- | Solve a binding group's constraints.
--
-- Arity is never guessed: a call can only be typed once its head's arity
-- is known, since with n arguments to a head of arity m it's a partial
-- application (n < m: the call's type is an arrow over the rest), a full
-- one (n == m), or an error (n > m: over-application, rejected even if the
-- head's result is itself a function). So all the equalities are unified
-- first, and then each call whose head is now known to be an arrow is
-- resolved, its own equalities unified in turn -- which can fix another
-- call's head -- until nothing changes. A call whose head is still a
-- metavariable then is ambiguous, and rejected: nothing says how many
-- arguments it takes.
--
-- Resolving a call only removes it and never adds one, so this terminates,
-- and the answer doesn't depend on the order of the calls.
solveGroup :: (IsString s, Show s, Ord s) => [Constraint s] -> Either s (Subst s)
solveGroup cs = do
    su <- unifyAll (Subst mempty) [ (a, b) | Equal a b <- cs ]
    loop su [ (o, h, as, r) | AppC o h as r <- cs ]

    where
    loop su pending = do
        (su', stuck) <- resolveAll su pending
        case stuck of
            [] -> pure su'
            _ | length stuck < length pending -> loop su' stuck
            (o, _, as, _) : _ -> Left $ fromString [iii|
                ambiguous arity: cannot determine how many parameters #{o :: String}
                takes (it is applied to #{plural (length as) "argument"},
                but nothing fixes it to a function type)
            |]

    -- One pass over the pending calls, resolving all it can, in order.
    resolveAll su [] = pure (su, [])
    resolveAll su (c@(o, h, as, r) : rest) =
        case substituteType su h of
            TyVar _ -> do
                (su', stuck) <- resolveAll su rest
                pure (su', c : stuck)
            TyArr ps res
                | n > m -> Left $ fromString [iii|
                      over-application: #{o :: String} has type #{renderType (TyArr ps res)},
                      so takes #{plural m "parameter"}, but is given
                      #{plural n "argument"}; to call the function it returns,
                      nest the applications
                  |]
                | otherwise -> do
                      su' <- unifyAll su ((r, tyArr (drop n ps) res) : zip ps as)
                      resolveAll su' rest
                where n = length as
                      m = length ps
            other -> Left $ fromString [iii|
                not a function: #{o :: String} has type #{renderType other},
                but is applied to #{plural (length as) "argument"}
            |]

-- | Unify each pair, under (and extending) a substitution.
unifyAll :: (IsString s, Show s, Ord s) => Subst s -> [(Type s, Type s)] -> Either s (Subst s)
unifyAll = go
    where
    go su [] = pure su
    go su ((a, b) : rest) = do
        su1 <- unifies (substituteType su a) (substituteType su b)
        go (su1 `compose` su) rest

compose :: Ord s => Subst s -> Subst s -> Subst s
(Subst s1) `compose` (Subst s2) = Subst $ M.map (substituteType (Subst s1)) s2 `M.union` s1

unifyMany :: (IsString s, Show s, Ord s) => [Type s] -> [Type s] -> Either s (Subst s)
unifyMany [] [] = pure $ Subst mempty
unifyMany (t1 : ts1) (t2 : ts2) =
  do su1 <- unifies t1 t2
     su2 <- unifyMany (map (substituteType su1) ts1)
                      (map (substituteType su1) ts2)
     return (su2 `compose` su1)
unifyMany _ _ = Left "unification mismatch"

unifies :: (IsString s, Ord s, Show s) => Type s -> Type s -> Either s (Subst s)
unifies t1 t2 | t1 == t2 = pure (Subst mempty)
unifies (TyVar v) t = v `bind` t
unifies t (TyVar v) = v `bind` t
unifies t1@(TyArr ps r) t2@(TyArr qs t)
    | length ps /= length qs =
        Left $ fromString [iii|
            arity mismatch: #{renderType t1} (#{plural (length ps) "parameter"})
            vs #{renderType t2} (#{plural (length qs) "parameter"})
        |]
    | otherwise =
        unifyMany (r : ps) (t : qs)

unifies (TyApp f1 x1) (TyApp f2 x2) = unifyMany [f1, x1] [f2, x2]

unifies t1 t2 = Left $ fromString [iii|Unification fail: #{renderType t1} <-> #{renderType t2}|]

bind :: (IsString s, Ord s, Show s) => s -> Type s -> Either s (Subst s)
bind a t | t == TyVar a    = pure (Subst mempty)
         | occursCheck a t = Left $ fromString [iii|Infinite type: #{renderType (TyVar a)} ~ #{renderType t}|]
         | otherwise       = pure (Subst $ M.singleton a t)

occursCheck :: Ord a => a -> Type a -> Bool
occursCheck a t = a `S.member` freeInType t

freeInType :: Ord s => Type s -> Set s
freeInType TyCon{}         = S.empty
freeInType (TyVar a)       = S.singleton a
freeInType (TyArr ps r)    = foldMap freeInType ps `S.union` freeInType r
freeInType (t1 `TyApp` t2) = freeInType t1 `S.union` freeInType t2

plural :: Int -> String -> String
plural 1 noun = [iii|1 #{noun}|]
plural n noun = [iii|#{n} #{noun}s|]
