module Main (main) where

import qualified Parse.ExpressionTest
import qualified Transform.AnfTest
import qualified Transform.ClosureConvertTest
import qualified Transform.EtaSaturateTest
import qualified Transform.HoistTest
import qualified Transform.MonomorphiseTest
import qualified TypeSystem.InferTest

import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "compiler-lib"
    [ Parse.ExpressionTest.tests
    , TypeSystem.InferTest.tests
    , Transform.MonomorphiseTest.tests
    , Transform.EtaSaturateTest.tests
    , Transform.ClosureConvertTest.tests
    , Transform.HoistTest.tests
    , Transform.AnfTest.tests
    ]
