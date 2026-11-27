module Transform.Anf ( CExpr (..)
                     , NExpr (..)
                     , NFun (..)
                     , NModule (..)
                     , anf
                     ) where

import Core.Expression (Op, Pattern (..), Term (..), patternVars)
import Transform.Hoist (HExpr (..), HFun (..), HModule (..))

import           Control.Monad.Trans.RWS.Strict (RWST, ask, asks, evalRWST, get, gets, local, modify, put)
import           Data.Map.Strict                (Map)
import qualified Data.Map.Strict as M
import           Data.Set                       (Set)
import qualified Data.Set as S
import           Data.String                    (IsString (..))

newtype NModule t s =
    NModule [NFun t s]
        deriving Show

data NFun t s =
    NFun !s !(Set s) ![s] !(NExpr t s)
        deriving (Eq, Show)

data NExpr t s
    = AExpr !(Term t s)
    | CExpr !(CExpr t s)
    | NLet !t !s !(NExpr t s) !(NExpr t s)
    | NLetRec ![(t, s, NExpr t s)] !(NExpr t s)
        deriving (Eq, Show)

data CExpr t s
    = CApp !t !(Term t s) ![Term t s]
    | CMkClo !t !(Set s) !s
    | CDataCon !t !s ![Term t s]
    | CCase !t !(Term t s) ![(Pattern s, NExpr t s)]
    | CBinOp !t !Op !(Term t s) !(Term t s)
        deriving (Eq, Show)

data NState s =
    NState { _counter :: !Int
           , used     :: !(Set s)
           }

type Anf s m a =
    RWST (Map s s) () (NState s) m a

anf :: (Monad m, Ord s, IsString s) => HModule t s -> m (NModule t s)
anf (HModule fs) =
    NModule . fst <$> evalRWST (traverse (nFun globals) fs) M.empty (NState 0 S.empty)
    where
    globals = S.fromList [n | HFun n _ _ _ <- fs]

nFun :: (Monad m, Ord s, IsString s)
     => Set s -> HFun t s -> Anf s m (NFun t s)
nFun globals (HFun n fvs vs expr) = do
    modify $ \st -> st { used = S.unions [globals, fvs, S.fromList vs] }
    NFun n fvs vs <$> normalise expr

normalise :: (Monad m, Ord s, IsString s) => HExpr t s -> Anf s m (NExpr t s)
normalise expr = asAnfExpr expr (pure . valueExpr)

asAnfExpr :: (Monad m, Ord s, IsString s)
          => HExpr t s
          -> (Value t s -> Anf s m (NExpr t s))
          -> Anf s m (NExpr t s)
asAnfExpr expr k =

    case expr of

        HTerm (Var ty v) -> do
            v' <- renamed v
            k (Atomic $ Var ty v')

        HTerm t ->
            k (Atomic t)

        HApp ty f xs ->
            asAtomicExpr f $ \f' ->
                asAtomicExprs xs $ \xs' ->
                    k (Complex $ CApp ty f' xs')

        HMkClo ty fvs f -> do
            fvs' <- S.fromList <$> traverse renamed (S.toList fvs)
            k (Complex $ CMkClo ty fvs' f)

        HLet bs body -> do
            outer <- ask
            let k' v = local (const outer) (k v)
            asLet bs body k'

        HDataCon ty c xs ->
            asAtomicExprs xs $ \xs' ->
                k (Complex $ CDataCon ty c xs')

        HCase ty scrut alts ->
            asAtomicExpr scrut $ \scrut' -> do
                alts' <- traverse nAlt alts
                k (Complex $ CCase ty scrut' alts')

        HBinOp ty op a b ->
            asAtomicExpr a $ \a' ->
                asAtomicExpr b $ \b' ->
                    k (Complex $ CBinOp ty op a' b')

nAlt :: (Monad m, Ord s, IsString s)
     => (Pattern s, HExpr t s)
     -> Anf s m (Pattern s, NExpr t s)
nAlt (pat, body) = do
    let vs = patternVars pat
    vs' <- traverse bind vs
    let rs = zip vs vs'
    withRenames rs $
        (,) (renamePattern (M.fromList rs) pat) <$> normalise body

renamePattern :: Ord s => Map s s -> Pattern s -> Pattern s
renamePattern rs = go
    where
    go = \case
        PVar v        -> PVar (M.findWithDefault v v rs)
        PDataCon c ps -> PDataCon c (map go ps)
        p             -> p

asLet :: (Monad m, Ord s, IsString s)
      => [(t, s, HExpr t s)]
      -> HExpr t s
      -> (Value t s -> Anf s m (NExpr t s))
      -> Anf s m (NExpr t s)
asLet bs body k

    | any (\(_, _, b) -> any (`mentionedIn` b) names) bs = do
        names' <- traverse bind names
        withRenames (zip names names') $ do
            bs' <- traverse (\((t, _, b), n') -> (,,) t n' <$> normalise b)
                            (zip bs names')
            NLetRec bs' <$> asAnfExpr body k

    | otherwise =
        nonRec bs []

    where
    names = [n | (_, n, _) <- bs]

    nonRec [] rs =
        withRenames rs (asAnfExpr body k)
    nonRec ((t, n, b) : rest) rs =
        asAnfExpr b $ \v -> do
            n' <- bind n
            NLet t n' (valueExpr v) <$> nonRec rest ((n, n') : rs)

asAtomicExpr :: (Monad m, Ord s, IsString s)
             => HExpr t s
             -> (Term t s -> Anf s m (NExpr t s))
             -> Anf s m (NExpr t s)
asAtomicExpr expr k =
    asAnfExpr expr $ \case
        Atomic a  -> k a
        Complex c -> do
            s <- fresh
            let ty = cType c
            NLet ty s (CExpr c) <$> k (Var ty s)

asAtomicExprs :: (Monad m, Ord s, IsString s)
              => [HExpr t s]
              -> ([Term t s] -> Anf s m (NExpr t s))
              -> Anf s m (NExpr t s)
asAtomicExprs [] k = k []
asAtomicExprs (e:es) k =
    asAtomicExpr e $ \e' ->
        asAtomicExprs es $ \es' ->
            k (e':es')

bind :: (Monad m, Ord s, IsString s) => s -> Anf s m s
bind n = do
    taken <- gets (S.member n . used)
    if taken
        then fresh
        else n <$ modify (\st -> st { used = S.insert n (used st) })

fresh :: (Monad m, Ord s, IsString s) => Anf s m s
fresh = do
    NState n u <- get
    let s = fromString ("anf_" <> show n)
    put $ NState (n + 1) (S.insert s u)
    pure s

renamed :: (Monad m, Ord s) => s -> Anf s m s
renamed v = asks (M.findWithDefault v v)

withRenames :: (Monad m, Ord s) => [(s, s)] -> Anf s m a -> Anf s m a
withRenames rs = local (M.union (M.fromList rs))

data Value t s
    = Atomic  !(Term t s)
    | Complex !(CExpr t s)

valueExpr :: Value t s -> NExpr t s
valueExpr = \case
    Atomic a  -> AExpr a
    Complex c -> CExpr c

cType :: CExpr t s -> t
cType = \case
    CApp t _ _     -> t
    CMkClo t _ _   -> t
    CDataCon t _ _ -> t
    CCase t _ _    -> t
    CBinOp t _ _ _ -> t

mentionedIn :: Ord s => s -> HExpr t s -> Bool
mentionedIn a = go

    where
    go = \case
        HTerm (Var _ v) -> v == a
        HTerm _         -> False
        HApp _ f xs     -> any go (f : xs)
        HMkClo _ fvs _  -> a `S.member` fvs
        HLet bs body    -> a `notElem` [n | (_, n, _) <- bs]
                        && (any (\(_, _, b) -> go b) bs || go body)
        HDataCon _ _ xs -> any go xs
        HCase _ s alts  -> go s || any (\(p, b) -> a `notElem` patternVars p && go b) alts
        HBinOp _ _ x y  -> go x || go y
