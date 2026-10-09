module Pretty.Expression ( buildExpr
                         , buildTerm
                         ) where

import Core.Expression (Expr (..), Term (..))

import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8   as C8
import           Data.List               (intersperse)

buildExpr :: (s -> ByteString) -> Expr t s -> Builder
buildExpr toBS = block 0

    where
    block i = \case

        Let bs e ->
            "let " <> sepBy (newline (i + 4)) (map (blockBinding (i + 4)) bs)
                <> newline i <> "in " <> block (i + 3) e

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

        Let bs e ->
            parensIf (p > Top) $
                "let " <> sepBy "; " (map inlineBinding bs)
                    <> " in " <> inline Top e

    blockBinding i (_, f, e1) =
        let (binder, rhs) = letParts f e1
        in binder <> " =" <> blockTail i rhs

    inlineBinding (_, f, e1) =
        let (binder, rhs) = letParts f e1
        in binder <> " = " <> inline Top rhs

    letParts f (Lam _ xs body) = (names (f:xs), body)
    letParts f e1              = (name toBS f, e1)

    lamHead xs = "\\" <> names xs <> " ->"

    names = sepBy " " . map (name toBS)

    newline i = "\n" <> B.string7 (replicate i ' ')

buildTerm :: (s -> ByteString) -> Term t s -> Builder
buildTerm toBS = \case
    Var _ v     -> name toBS v
    LitInt n    -> B.intDec n
    LitBool b   -> if b then "True" else "False"
    LitString s -> B.string7 (show (C8.unpack (toBS s)))

name :: (s -> ByteString) -> s -> Builder
name toBS = B.byteString . toBS

sepBy :: Builder -> [Builder] -> Builder
sepBy sep = mconcat . intersperse sep

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
