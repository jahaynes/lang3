module TypeSystem.InferTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Pipeline               (compile)
import TypeSystem.Common      (typeOf)
import TypeSystem.InferModule
import TypeSystem.Types

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.List             (isInfixOf)

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "Infer"

    [ testCase "integer literal" $
        typesTo [("main", "1")] [("main", "Int")]

    , testCase "string literal" $
        typesTo [("main", "\"hi\"")] [("main", "String")]

    , testCase "identity is polymorphic" $
        typesTo [("id", "\\x. x")] [("id", "a0 -> a0")]

    , testCase "two-argument function" $
        typesTo [("k", "\\x y. x")] [("k", "(a0, b0) -> a0")]

    , testCase "top-level functions are instantiated at each use" $
        typesTo [ ("id", "\\x. x"), ("k", "\\x y. x"), ("main", "k (id 1) (id True)") ]
                [ ("id", "a0 -> a0"), ("k", "(a0, b0) -> a0"), ("main", "Int") ]

    , testCase "partial application gives an arrow over the rest" $
        typesTo [ ("k", "\\x y. x"), ("main", "k 1") ]
                [ ("k", "(a0, b0) -> a0"), ("main", "a0 -> Int") ]

    , testCase "a function's arity is part of its type" $
        typesTo [("main", "let apply = \\f. f 1 2 in apply (\\a b. a)")] [("main", "Int")]

    , testCase "let-bound recursive function" $
        typesTo [("main", "let f x = f x in f 1")] [("main", "a0")]

    , testCase "mutually recursive top-level functions" $
        typesTo [ ("ping", "\\n. pong n"), ("pong", "\\n. ping n"), ("main", "ping 1") ]
                [ ("ping", "a0 -> b0"), ("pong", "a0 -> b0"), ("main", "a0") ]

    , testCase "a function may refer to one defined after it" $
        typesTo [ ("main", "id 1"), ("id", "\\x. x") ]
                [ ("main", "Int"), ("id", "a0 -> a0") ]

    , testCase "three-way mutual recursion, used from outside the cycle" $
        typesTo [ ("main", "a 1")
                , ("a", "\\n. b n")
                , ("b", "\\n. c n")
                , ("c", "\\n. a n")
                , ("id", "\\x. x")
                , ("d", "\\n. id (a n)") ]
                [ ("main", "a0"), ("a", "a0 -> b0"), ("b", "a0 -> b0"), ("c", "a0 -> b0")
                , ("id", "a0 -> a0"), ("d", "a0 -> b0") ]

    , testCase "two cycles, one depending on the other" $
        typesTo [ ("p", "\\n. q (r n)"), ("q", "\\n. p n")
                , ("r", "\\n. s n"), ("s", "\\n. r 1") ]
                [ ("p", "Int -> a0"), ("q", "Int -> a0")
                , ("r", "Int -> a0"), ("s", "Int -> a0") ]

    , testCase "data constructors and case" $
        typesModuleTo [listDefn]
            [ ("main", eCase (DataCon () "Cons" [int 1, DataCon () "Nil" []])
                         [ (PDataCon "Cons" [PVar "x", PWild], eVar "x")
                         , (PDataCon "Nil" [], int 0) ]) ]
            [ ("main", "Int") ]

    , testCase "nullary constructor is polymorphic" $
        typesModuleTo [listDefn] [("main", DataCon () "Nil" [])] [("main", "List a0")]

    , testCase "every node is annotated" $
        annotatesTo [("id", "\\x. x"), ("main", "id 1")] "main" $
            App typeInt (Term (Var (TyArr [typeInt] typeInt) "id")) [Term (LitInt 1)]

    , testCase "over-application is rejected" $
        failsWith [("k", "\\x y. x"), ("main", "k 1 2 3")] "over-application"

    , testCase "applying a literal is rejected" $
        failsWith [("main", "1 2")] "not a function"

    , testCase "unbound variable is rejected" $
        failsWith [("main", "f 1")] "unbound variable: f"

    , testCase "unknown arity is rejected" $
        failsWith [("main", "\\f. f 1")] "ambiguous arity"

    , testCase "passing a function of too few parameters is rejected" $
        failsWith [("main", "let apply = \\f. f 1 2 in apply (\\x. x)")] "over-application"

    , testCase "arity mismatch is rejected" $
        failsModuleWith [listDefn]
            [ ("main", DataCon () "Cons" [ Lam () ["x"] (eVar "x")
                                         , DataCon () "Cons" [ Lam () ["x", "y"] (eVar "x")
                                                             , DataCon () "Nil" [] ] ]) ]
            "arity mismatch"

    , testCase "constructor field count is checked" $
        failsModuleWith [listDefn] [("main", DataCon () "Cons" [int 1])] "expects 2 field(s)"

    , testCase "arithmetic is on Ints" $
        typesTo [("main", "\\x y. x + y * 2")] [("main", "(Int, Int) -> Int")]

    , testCase "logic is on Bools" $
        typesTo [("main", "\\x y. x && y || True")] [("main", "(Bool, Bool) -> Bool")]

    , testCase "equality is polymorphic and gives a Bool" $
        typesTo [("eq", "\\x y. x == y")] [("eq", "(a0, a0) -> Bool")]

    , testCase "equality's operands must have the same type" $
        failsWith [("main", "1 == True")] "Unification fail"

    , testCase "arithmetic on a Bool is rejected" $
        failsWith [("main", "1 + True")] "Unification fail"

    , testCase "logic on an Int is rejected" $
        failsWith [("main", "True && 1")] "Unification fail"

    , testCase "a binop is annotated with its result type" $
        annotatesTo [("main", "1 == 2")] "main" $
            BinOp typeBool Equals (Term (LitInt 1)) (Term (LitInt 2))

    , testCase "a function called in a binop is found in the call graph" $
        typesTo [ ("main", "inc 1 + 1"), ("inc", "\\n. n + 1") ]
                [ ("main", "Int"), ("inc", "Int -> Int") ]

    , testCase "pipeline runs on a typed module" $
        case compile [] =<< parseModule [("id", "\\x. x"), ("main", "id 1")] of
            Left err -> assertFailure (C8.unpack err)
            Right _  -> pure ()
    ]

listDefn :: DataDefn ByteString
listDefn = DataDefn "List" ["a"]
    [ DCon "Nil" []
    , DCon "Cons" [TyVar "a", TyApp (TyCon "List") (TyVar "a")] ]

parseModule :: [(ByteString, ByteString)] -> Either ByteString (Module (Expr () ByteString) ByteString)
parseModule defs = Module <$> traverse (\(n, src) -> Fun n <$> runUntypedExpr src) defs

typesTo :: [(ByteString, ByteString)] -> [(ByteString, String)] -> Assertion
typesTo defs expected =
    either (assertFailure . ("parse error: " <>) . C8.unpack)
           (\md -> typesModuleTo [] [ (n, e) | Fun n e <- getFunDefns md ] expected)
           (parseModule defs)

typesModuleTo :: [DataDefn ByteString] -> [(ByteString, Expr () ByteString)] -> [(ByteString, String)] -> Assertion
typesModuleTo dataDefns defs expected =
    case inferModule dataDefns (Module [ Fun n e | (n, e) <- defs ]) of
        Left err -> assertFailure $ "type error: " <> C8.unpack err
        Right md -> [ (n, renderType (typeOf e)) | Fun n e <- getFunDefns md ] @?= expected

annotatesTo :: [(ByteString, ByteString)] -> ByteString -> Expr (Type ByteString) ByteString -> Assertion
annotatesTo defs name expected =
    case inferModule [] =<< parseModule defs of
        Left err -> assertFailure (C8.unpack err)
        Right md -> lookup name [ (n, e) | Fun n e <- getFunDefns md ] @?= Just expected

failsWith :: [(ByteString, ByteString)] -> String -> Assertion
failsWith defs msg =
    either (assertFailure . ("parse error: " <>) . C8.unpack)
           (\md -> failsModuleWith [] [ (n, e) | Fun n e <- getFunDefns md ] msg)
           (parseModule defs)

failsModuleWith :: [DataDefn ByteString] -> [(ByteString, Expr () ByteString)] -> String -> Assertion
failsModuleWith dataDefns defs msg =
    case inferModule dataDefns (Module [ Fun n e | (n, e) <- defs ]) of
        Left err | msg `isInfixOf` C8.unpack err -> pure ()
                 | otherwise -> assertFailure $ "wrong error: " <> C8.unpack err
        Right md -> assertFailure $ "expected a type error, got: "
                                 <> show [ (n, renderType (typeOf e)) | Fun n e <- getFunDefns md ]

eVar :: ByteString -> Expr () ByteString
eVar = Term . Var ()

int :: Int -> Expr () ByteString
int = Term . LitInt

eCase :: Expr () ByteString -> [(Pattern ByteString, Expr () ByteString)] -> Expr () ByteString
eCase = Case ()
