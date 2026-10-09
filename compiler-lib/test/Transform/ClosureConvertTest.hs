module Transform.ClosureConvertTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Pretty.Class
import Transform.ClosureConvert

import Data.ByteString         (ByteString)
import Data.Functor.Identity   (Identity (..))
import Data.String.Interpolate (i, iii)

tests :: IO ()
tests = do
    
    let Right parsed = runUntypedExpr program1

    let m = Module [Fun "program1" parsed] :: Module (Expr () ByteString) ByteString

    let Identity m' = closureConvert m :: Identity (Module (CCExpr () ByteString) ByteString)

    let Module [Fun "program1" expr'] = m'

    putStrLn [i|#{pretty parsed}|]

    print expr'


program1 :: ByteString
program1 = [iii|
f (g x)
|]
