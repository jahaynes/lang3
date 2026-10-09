module Transform.ClosureConvertTest ( tests ) where

import Parse.LexAndParse

import Data.ByteString         (ByteString)
import Data.String.Interpolate (iii)

tests :: IO ()
tests = do
    
    let Right parsed = runUntypedExpr program1

    print parsed

program1 :: ByteString
program1 = [iii|
f (g x)
|]
