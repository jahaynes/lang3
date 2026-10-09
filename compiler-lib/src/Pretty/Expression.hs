module Pretty.Expression ( buildExpr
                         , buildTerm
                         ) where

import Core.Expression (Expr (..), Term (..))

import           Control.Monad.Trans.State.Strict (State, evalState, get, modify', put)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Builder          as B
import qualified Data.ByteString.Char8            as C8

buildExpr :: (s -> ByteString) -> Expr t s -> Builder
buildExpr toBS e = evalState (go e) 0

    where
    go = \case

        Term t ->
            str (termBS toBS t)

        App _ f xs ->
            cat $ parensIf (needsParensFun f) (go f)
                : map (\x -> cat [str " ", parensIf (needsParensArg x) (go x)]) xs

        Lam _ xs body ->
            cat [str "\\", names xs, str " -> ", go body]

        Let _ f e1 e2 -> do
            let (binder, rhs) = letParts f e1
            c <- get
            cat [str "let ", binder, str " = ", go rhs, newline c, str "in ", go e2]

    letParts f (Lam _ xs body) = (names (f:xs), body)
    letParts f e1              = (str (toBS f), e1)

    names = str . C8.unwords . map toBS

buildTerm :: (s -> ByteString) -> Term t s -> Builder
buildTerm toBS = B.byteString . termBS toBS

termBS :: (s -> ByteString) -> Term t s -> ByteString
termBS toBS = \case
    Var _ v     -> toBS v
    LitInt n    -> C8.pack (show n)
    LitBool b   -> if b then "True" else "False"
    LitString s -> C8.pack (show (C8.unpack (toBS s)))

str :: ByteString -> State Int Builder
str s = B.byteString s <$ modify' (+ BS.length s)

newline :: Int -> State Int Builder
newline c = ("\n" <> B.string7 (replicate c ' ')) <$ put c

cat :: [State Int Builder] -> State Int Builder
cat = fmap mconcat . sequence

parensIf :: Bool -> State Int Builder -> State Int Builder
parensIf True  b = cat [str "(", b, str ")"]
parensIf False b = b

needsParensFun :: Expr t s -> Bool
needsParensFun = \case
    Term t -> isNegative t
    App {} -> False
    Lam {} -> True
    Let {} -> True

needsParensArg :: Expr t s -> Bool
needsParensArg = \case
    Term t -> isNegative t
    _      -> True

isNegative :: Term t s -> Bool
isNegative (LitInt n) = n < 0
isNegative _          = False
