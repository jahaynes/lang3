module Pretty.Expression ( buildExpr
                         , buildTerm
                         ) where

import Core.Expression (Expr (..), Term (..))

import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C8

buildExpr :: (s -> ByteString) -> Expr t s -> Builder
buildExpr toBS = block 0

    where
    block i = \case

        Let _ f e1 e2 ->
            let (binder, rhs) = letParts f e1
            in "let " <> binder <> " =" <> blockTail (i + 4) rhs
                <> newline i <> "in " <> block (i + 3) e2

        Lam _ xs body ->
            lamHead xs <> blockTail (i + 4) body

        e -> inline Top e

    blockTail i e
        | isBlock e = newline i <> block i e
        | otherwise = " " <> inline Top e

    inline p = \case

        Term t ->
            parensIf (p > Top && isNegative t) (buildTerm toBS t)

        App _ f xs ->
            parensIf (p > FunPos) $
                inline FunPos f <> foldMap (\x -> " " <> inline ArgPos x) xs

        Lam _ xs body ->
            parensIf (p > Top) $
                lamHead xs <> " " <> inline Top body

        Let _ f e1 e2 ->
            let (binder, rhs) = letParts f e1
            in parensIf (p > Top) $
                   "let " <> binder <> " = " <> inline Top rhs
                       <> " in " <> inline Top e2

    letParts f (Lam _ xs body) = (names (f:xs), body)
    letParts f e1              = (name toBS f, e1)

    lamHead xs = "\\" <> names xs <> " ->"

    names = mconcat . spaced . map (name toBS)

    spaced []     = []
    spaced (x:xs) = x : map (" " <>) xs

    newline i = "\n" <> B.string7 (replicate i ' ')

buildTerm :: (s -> ByteString) -> Term t s -> Builder
buildTerm toBS = \case
    Var _ v     -> name toBS v
    LitInt n    -> B.intDec n
    LitBool b   -> if b then "True" else "False"
    LitString s -> B.string7 (show (C8.unpack (toBS s)))

name :: (s -> ByteString) -> s -> Builder
name toBS = B.byteString . toBS

parensIf :: Bool -> Builder -> Builder
parensIf True  b = "(" <> b <> ")"
parensIf False b = b

data Prec = Top | FunPos | ArgPos
    deriving (Eq, Ord)

isNegative :: Term t s -> Bool
isNegative (LitInt n) = n < 0
isNegative _          = False

isBlock :: Expr t s -> Bool
isBlock = \case
    Let {}       -> True
    Lam _ _ body -> isBlock body
    _            -> False
