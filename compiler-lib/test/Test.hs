module Main (main) where

import qualified Transform.ClosureConvertTest

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "compiler-lib"
    [ Transform.ClosureConvertTest.tests
    ]
