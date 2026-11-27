module Parse.ExpressionTest (tests) where

import Core.Expression
import Parse.LexAndParse

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as C8

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "Parse.Expression"

    [ testCase "simple binop" $
        parsesTo "x + 1"
            (BinOp () Plus (var "x") (int 1))

    , testCase "each operator" $ do
        parsesTo "a + b"  (BinOp () Plus   (var "a") (var "b"))
        parsesTo "a * b"  (BinOp () Times  (var "a") (var "b"))
        parsesTo "a == b" (BinOp () Equals (var "a") (var "b"))
        parsesTo "a && b" (BinOp () And    (var "a") (var "b"))
        parsesTo "a || b" (BinOp () Or     (var "a") (var "b"))

    , testCase "operators are left-associative" $
        parsesTo "a + b + c"
            (BinOp () Plus (BinOp () Plus (var "a") (var "b")) (var "c"))

    , testCase "times binds tighter than plus" $ do
        parsesTo "a + b * c"
            (BinOp () Plus (var "a") (BinOp () Times (var "b") (var "c")))
        parsesTo "a * b + c"
            (BinOp () Plus (BinOp () Times (var "a") (var "b")) (var "c"))

    , testCase "full precedence ladder" $
        parsesTo "a || b && c == d + e * f"
            (BinOp () Or (var "a")
                (BinOp () And (var "b")
                    (BinOp () Equals (var "c")
                        (BinOp () Plus (var "d")
                            (BinOp () Times (var "e") (var "f"))))))

    , testCase "application binds tighter than operators" $
        parsesTo "f x + g y"
            (BinOp () Plus (App () (var "f") [var "x"]) (App () (var "g") [var "y"]))

    , testCase "parentheses override precedence" $
        parsesTo "(a + b) * c"
            (BinOp () Times (BinOp () Plus (var "a") (var "b")) (var "c"))

    , testCase "parenthesised binop as an argument" $
        parsesTo "f (x + 1)"
            (App () (var "f") [BinOp () Plus (var "x") (int 1)])

    , testCase "negative literal operand" $
        parsesTo "x + -1"
            (BinOp () Plus (var "x") (int (-1)))

    , testCase "lambda body extends over operators" $
        parsesTo "\\x. x + 1"
            (Lam () ["x"] (BinOp () Plus (var "x") (int 1)))

    , testCase "binops in let binding and body" $
        parsesTo "let y = x * 2 in y + 1"
            (Let [((), "y", BinOp () Times (var "x") (int 2))]
                 (BinOp () Plus (var "y") (int 1)))

    , testCase "literal booleans" $
        parsesTo "True && False"
            (BinOp () And (Term (LitBool True)) (Term (LitBool False)))
    ]

parsesTo :: ByteString -> Expr () ByteString -> Assertion
parsesTo src expected =
    case runUntypedExpr src of
        Left err -> assertFailure $ "parse error: " <> C8.unpack err
        Right e  -> e @?= expected

var :: ByteString -> Expr () ByteString
var = Term . Var ()

int :: Int -> Expr () ByteString
int = Term . LitInt
