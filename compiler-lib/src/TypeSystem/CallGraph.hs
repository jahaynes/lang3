module TypeSystem.CallGraph ( CallGraph (..)
                            , createPlan
                            ) where

import           Data.Graph      (flattenSCC, stronglyConnComp)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Set        (Set)
import qualified Data.Set as S

-- | Each function, and the functions it refers to.
newtype CallGraph s =
    CallGraph (Map s (Set s))
        deriving (Eq, Show)

-- | The order to infer functions in: one binding group at a time, each a
-- strongly connected component of the call graph (a set of mutually
-- recursive functions, or a single function), with every group coming
-- after the groups it refers to.
--
-- Edges to names that aren't in the graph are ignored.
createPlan :: Ord s => CallGraph s -> [Set s]
createPlan (CallGraph cg) =
    map (S.fromList . flattenSCC) . stronglyConnComp $
        [ (n, n, S.toList deps) | (n, deps) <- M.toList cg ]
