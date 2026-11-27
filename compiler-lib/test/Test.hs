module Main (main) where

import qualified Parse.ExpressionTest
import qualified Transform.AnfTest
import qualified Transform.ClosureConvertTest
import qualified Transform.HoistTest

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "compiler-lib"
    [ Parse.ExpressionTest.tests
    , Transform.ClosureConvertTest.tests
    , Transform.HoistTest.tests
    , Transform.AnfTest.tests
    ]
