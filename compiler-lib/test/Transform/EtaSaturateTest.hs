module Transform.EtaSaturateTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Transform.EtaSaturate
import Transform.Monomorphise
import TypeSystem.InferModule

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List             (intercalate)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

-- Expected results are written in a shape that shows every call's arity:
-- f(x, y) passes two arguments at once, f(x)(y) passes one and then one,
-- and f() is a call with none.
tests :: TestTree
tests = testGroup "EtaSaturate"

    [ testCase "saturated call is unchanged" $
        saturatesTo [("main", "let k = \\x y. x in k 1 2")] "main"
            "let k = (\\x y -> x) in k(1, 2)"

    , testCase "partial call becomes a lambda over the rest" $
        saturatesTo [("main", "let k = \\x y. x in k 1")] "main"
            "let k = (\\x y -> x) in (\\eta$0 -> k(1, eta$0))"

    , testCase "non-value argument is let-bound outside the new lambda" $
        saturatesTo [("main", "let k = \\x y. x in let id = \\z. z in k (id 1)")] "main"
            "let k = (\\x y -> x) in let id = (\\z -> z) in \
            \let eta$0 = id(1) in (\\eta$1 -> k(eta$0, eta$1))"

    , testCase "a partial call that is itself called is one call" $
        saturatesTo [("main", "let k = \\x y. x in (k 1) 2")] "main"
            "let k = (\\x y -> x) in k(1, 2)"

    , testCase "the function a call returns is called separately" $
        saturatesTo [("main", "let f = \\x. \\y. x in (f 1) 2")] "main"
            "let f = (\\x -> (\\y -> x)) in f(1)(2)"

    , testCase "partial call as an argument" $
        saturatesTo [("main", "let k = \\x y. x in let ap = \\f. f 2 in ap (k 1)")] "main"
            "let k = (\\x y -> x) in let ap = (\\f -> f(2)) in ap((\\eta$0 -> k(1, eta$0)))"

    , testCase "0-parameter top level is called on reference" $
        saturatesTo [("one", "1"), ("main", "one")] "main" "one()"

    , testCase "0-parameter top level's result is then called" $
        saturatesTo [("k", "\\x y. x"), ("inc", "k 1"), ("main", "inc 2")] "main" "inc()(2)"

    , testCase "top level that saturates to a lambda returns it" $
        saturatesTo [("k", "\\x y. x"), ("inc", "k 1"), ("main", "inc 2")] "inc"
            "let eta$1 = (\\eta$0 -> k(1, eta$0)) in eta$1"

    , testCase "top-level lambda keeps its binders" $
        saturatesTo [("k", "\\x y. x"), ("main", "k 1 2")] "k" "(\\x y -> x)"

    , testCase "lambda-bound name hides a 0-parameter top level" $
        saturatesTo [("one", "1"), ("main", "(\\one. one) 2")] "main" "(\\one -> one)(2)"

    , testCase "let-bound name hides a 0-parameter top level" $
        saturatesTo [("one", "1"), ("main", "let one = 2 in one")] "main" "let one = 2 in one"

    , testCase "binop operands are saturated" $
        saturatesTo [("one", "1"), ("main", "one + one")] "main" "(one() + one())"

    , testCase "call in a binop operand is merged" $
        saturatesTo [("main", "let k = \\x y. x in (k 1) 2 * 3")] "main"
            "let k = (\\x y -> x) in (k(1, 2) * 3)"
    ]

parseModule :: [(ByteString, ByteString)] -> Either ByteString (Module (Expr () ByteString) ByteString)
parseModule defs = Module <$> traverse (\(n, src) -> Fun n <$> runUntypedExpr src) defs

saturatesTo :: [(ByteString, ByteString)] -> ByteString -> String -> Assertion
saturatesTo defs name expected =
    case etaSaturate =<< monomorphise =<< inferModule [] =<< parseModule defs of
        Left err -> assertFailure (C8.unpack err)
        Right md -> lookup name [ (unSpec n, shape e) | Fun n e <- getFunDefns md ] @?= Just expected

shape :: Expr t ByteString -> String
shape = \case
    Term (Var _ x)     -> unSpec' x
    Term (LitInt n)    -> show n
    Term (LitBool b)   -> show b
    Term (LitString s) -> show s
    App _ f xs         -> shape f ++ "(" ++ intercalate ", " (map shape xs) ++ ")"
    Lam _ vs b         -> "(\\" ++ unwords (map C8.unpack vs) ++ " -> " ++ shape b ++ ")"
    Let bs b           -> "let " ++ intercalate "; " [ C8.unpack n ++ " = " ++ shape e | (_, n, e) <- bs ]
                       ++ " in " ++ shape b
    DataCon _ c xs     -> C8.unpack c ++ "{" ++ intercalate ", " (map shape xs) ++ "}"
    Case _ s alts      -> "case " ++ shape s ++ " of {" ++ intercalate "; " [ show p ++ " -> " ++ shape e | (p, e) <- alts ] ++ "}"
    BinOp _ op a b     -> "(" ++ shape a ++ " " ++ opShape op ++ " " ++ shape b ++ ")"

    where
    unSpec' = C8.unpack . unSpec

    opShape = \case
        Plus   -> "+"
        Times  -> "*"
        Equals -> "=="
        And    -> "&&"
        Or     -> "||"

-- | A specialised copy's name without its type: k$(Int, Int) -> Int is k.
-- (A copy's type always renders with a space; eta's own names never do.)
unSpec :: ByteString -> ByteString
unSpec n =
    let (base, rest) = C8.break (== '$') n
    in if C8.elem ' ' rest then base else n
