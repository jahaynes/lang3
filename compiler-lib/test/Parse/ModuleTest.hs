module Parse.ModuleTest (tests) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import TypeSystem.Types

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as C8

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "Parse.Module"

    [ testCase "simple definition" $
        funsAre "x = 5" [Fun "x" (int 5)]

    , testCase "arguments become a lambda" $
        funsAre "f x y = x + y"
            [Fun "f" (Lam () ["x", "y"] (BinOp () Plus (var "x") (var "y")))]

    , testCase "definitions split at line starts" $
        funsAre "f x = g x\ng y = y\nmain = f 1"
            [ Fun "f"    (Lam () ["x"] (App () (var "g") [var "x"]))
            , Fun "g"    (Lam () ["y"] (var "y"))
            , Fun "main" (App () (var "f") [int 1])
            ]

    , testCase "indented continuation belongs to the definition" $
        funsAre "f x =\n  g\n    x\nh = 1"
            [ Fun "f" (Lam () ["x"] (App () (var "g") [var "x"]))
            , Fun "h" (int 1)
            ]

    , testCase "data definition" $
        dataDefnsAre "List a = Nil | Cons a (List a)"
            [DataDefn "List" ["a"]
                [ DCon "Nil" []
                , DCon "Cons" [TyVar "a", TyApp (TyCon "List") (TyVar "a")]
                ]]

    , testCase "data definition over several lines" $
        dataDefnsAre "Either a b\n  = Left a\n  | Right b\nBool2 = T | F"
            [ DataDefn "Either" ["a", "b"] [DCon "Left" [TyVar "a"], DCon "Right" [TyVar "b"]]
            , DataDefn "Bool2" [] [DCon "T" [], DCon "F" []]
            ]

    , testCase "nullary constructor doesn't take the next line" $
        dataDefnsAre "Unit = Unit\nOther = Other"
            [ DataDefn "Unit"  [] [DCon "Unit" []]
            , DataDefn "Other" [] [DCon "Other" []]
            ]

    , testCase "type signature" $
        typeSigsAre "f : Int"
            [TypeSig "f" (TyCon "Int")]

    , testCase "arrow chain is one n-ary arrow" $
        typeSigsAre "f : Int -> List a -> Bool"
            [TypeSig "f" (TyArr [TyCon "Int", TyApp (TyCon "List") (TyVar "a")] (TyCon "Bool"))]

    , testCase "parenthesised arrow is curried" $
        typeSigsAre "f : a -> (b -> c)"
            [TypeSig "f" (TyArr [TyVar "a"] (TyArr [TyVar "b"] (TyVar "c")))]

    , testCase "mixed module" $
        case runModule "Maybe a = Nothing | Just a\nid : a -> a\nid x = x\nmain = id 1" of
            Left err -> assertFailure $ "parse error: " <> C8.unpack err
            Right pm -> do
                getDataDefns pm @?= [DataDefn "Maybe" ["a"] [DCon "Nothing" [], DCon "Just" [TyVar "a"]]]
                getTypeSigs  pm @?= [TypeSig "id" (TyArr [TyVar "a"] (TyVar "a"))]
                getFunDefns pm @?=
                    [ Fun "id"   (Lam () ["x"] (var "x"))
                    , Fun "main" (App () (var "id") [int 1])
                    ]

    , testCase "unparseable definition is an error" $
        case runModule "f = 1\n= 2" of
            Left _   -> pure ()
            Right pm -> assertFailure $ "expected a parse error, got " <> show pm

    , testCase "indented definition is an error" $
        case runModule "  f = 1" of
            Left _   -> pure ()
            Right pm -> assertFailure $ "expected a parse error, got " <> show pm
    ]

parsed :: ByteString -> (Module (Expr () ByteString) ByteString -> Assertion) -> Assertion
parsed src k =
    case runModule src of
        Left err -> assertFailure $ "parse error: " <> C8.unpack err
        Right pm -> k pm

funsAre :: ByteString -> [Fun (Expr () ByteString) ByteString] -> Assertion
funsAre src expected = parsed src $ \pm -> getFunDefns pm @?= expected

dataDefnsAre :: ByteString -> [DataDefn ByteString] -> Assertion
dataDefnsAre src expected = parsed src $ \pm -> getDataDefns pm @?= expected

typeSigsAre :: ByteString -> [TypeSig ByteString] -> Assertion
typeSigsAre src expected = parsed src $ \pm -> getTypeSigs pm @?= expected

var :: ByteString -> Expr () ByteString
var = Term . Var ()

int :: Int -> Expr () ByteString
int = Term . LitInt
