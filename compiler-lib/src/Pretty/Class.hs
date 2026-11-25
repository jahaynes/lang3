module Pretty.Class ( Pretty (..) ) where

import Core.Expression   (Expr, Term)
import Pretty.Expression (buildExpr, buildTerm)

import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Lazy    as L
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder as B

class Pretty a where
    pretty :: a -> ByteString

instance Pretty ByteString where
    pretty = id

instance Pretty s => Pretty (Expr t s) where
    pretty = render . buildExpr pretty

instance Pretty s => Pretty (Term t s) where
    pretty = render . buildTerm pretty

render :: Builder -> ByteString
render = L.toStrict . B.toLazyByteString
