module Pretty.Expression ( buildExpr
                         , buildPattern
                         , buildTerm
                         ) where

import Core.Expression (Expr (..), Op (..), Pattern (..), Term (..))

import           Control.Monad.Trans.State.Strict (State, evalState, get, modify', put)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString as BS
import           Data.ByteString.Builder          (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C8
import           Data.List                        (intersperse)

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

        Let bs body -> do
            c <- get
            cat [ str "let ", sepBy (newline (c + 4)) (map binding bs)
                , newline c, str "in ", go body ]

        DataCon _ c xs ->
            cat $ str (toBS c) : map arg xs

        Case _ scrut alts -> do
            c <- get
            cat $ [str "case ", go scrut, str " of"]
               <> map (alt (c + 2)) alts

        BinOp _ op a b ->
            cat [operand a, str " ", str (opBS op), str " ", operand b]

    operand x = parensIf (needsParensOperand x) (go x)

    arg x = cat [str " ", parensIf (needsParensArg x) (go x)]

    alt c (p, body) =
        cat [newline c, str (patternBS toBS p), str " -> ", go body]

    binding (_, f, e1) =
        let (binder, rhs) = letParts f e1
        in cat [binder, str " = ", go rhs]

    letParts f (Lam _ xs body) = (names (f:xs), body)
    letParts f e1              = (str (toBS f), e1)

    names = str . C8.unwords . map toBS

opBS :: Op -> ByteString
opBS = \case
    Plus   -> "+"
    Times  -> "*"
    Equals -> "=="
    And    -> "&&"
    Or     -> "||"

buildPattern :: (s -> ByteString) -> Pattern s -> Builder
buildPattern toBS = B.byteString . patternBS toBS

patternBS :: (s -> ByteString) -> Pattern s -> ByteString
patternBS toBS = go
    where
    go = \case
        PLitInt n     -> C8.pack (show n)
        PLitBool b    -> if b then "True" else "False"
        PVar v        -> toBS v
        PDataCon c ps -> C8.unwords (toBS c : map sub ps)
        PWild         -> "_"

    sub p | needsParens p = "(" <> go p <> ")"
          | otherwise     = go p

    needsParens = \case
        PDataCon _ (_:_) -> True
        PLitInt n        -> n < 0
        _                -> False

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

sepBy :: State Int Builder -> [State Int Builder] -> State Int Builder
sepBy sep = cat . intersperse sep

parensIf :: Bool -> State Int Builder -> State Int Builder
parensIf True  b = cat [str "(", b, str ")"]
parensIf False b = b

needsParensFun :: Expr t s -> Bool
needsParensFun = \case
    Term t     -> isNegative t
    App {}     -> False
    Lam {}     -> True
    Let {}     -> True
    DataCon {} -> True
    Case {}    -> True
    BinOp {}   -> True

-- No precedence yet, so nested operators are always parenthesised.
needsParensOperand :: Expr t s -> Bool
needsParensOperand = \case
    Term t     -> isNegative t
    App {}     -> False
    DataCon {} -> False
    _          -> True

needsParensArg :: Expr t s -> Bool
needsParensArg = \case
    Term t         -> isNegative t
    DataCon _ _ [] -> False
    _              -> True

isNegative :: Term t s -> Bool
isNegative (LitInt n) = n < 0
isNegative _          = False
