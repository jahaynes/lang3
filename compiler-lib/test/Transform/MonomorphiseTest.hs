module Transform.MonomorphiseTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Transform.Monomorphise
import TypeSystem.Common        (mTypeOf)
import TypeSystem.InferModule
import TypeSystem.Types
import TypeSystem.TypesCallGraph (freeVars)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Set as S

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "Monomorphise"

    [ testCase "monomorphic module is kept as is" $
        monosTo [("main", "1")] [("main", "Int")]

    , testCase "uncalled generic is kept under its own name, defaulted" $
        monosTo [("id", "\\x. x")] [("id", "Int -> Int")]

    , testCase "generic is specialised once per instantiation" $
        monosTo [ ("id", "\\x. x"), ("k", "\\x y. x"), ("main", "k (id 1) (id True)") ]
                [ ("main", "Int")
                , ("k$(Int, Bool) -> Int", "(Int, Bool) -> Int")
                , ("id$Int -> Int", "Int -> Int")
                , ("id$Bool -> Bool", "Bool -> Bool") ]

    , testCase "repeated use at one instantiation shares a copy" $
        monosTo [ ("id", "\\x. x"), ("main", "id (id 1)") ]
                [ ("main", "Int"), ("id$Int -> Int", "Int -> Int") ]

    , testCase "generic root's own uses follow its defaulting" $
        monosTo [ ("k", "\\x y. x"), ("main", "k 1") ]
                [ ("main", "Int -> Int"), ("k$(Int, Int) -> Int", "(Int, Int) -> Int") ]

    , testCase "self-recursive generic refers to its own specialisation" $ do
        monosTo [ ("loop", "\\x. loop x"), ("main", "loop 1") ]
                [ ("main", "Int"), ("loop$Int -> Int", "Int -> Int") ]
        refersTo [ ("loop", "\\x. loop x"), ("main", "loop 1") ]
                 "loop$Int -> Int" ["loop$Int -> Int"]

    , testCase "mutually recursive generics specialise together" $
        monosTo [ ("ping", "\\n. pong n"), ("pong", "\\n. ping n"), ("main", "ping 1") ]
                [ ("main", "Int")
                , ("pong$Int -> Int", "Int -> Int")
                , ("ping$Int -> Int", "Int -> Int") ]

    , testCase "lambda-bound name hides a generic top level" $
        monosTo [ ("id", "\\x. x"), ("main", "(\\id. id 1) (\\y. y)") ]
                [ ("id", "Int -> Int"), ("main", "Int") ]

    , testCase "let-bound name hides a generic top level" $
        monosTo [ ("id", "\\x. x"), ("main", "let id = \\y. y in id 1") ]
                [ ("id", "Int -> Int"), ("main", "Int") ]
    ]

parseModule :: [(ByteString, ByteString)] -> Either ByteString (Module (Expr () ByteString) ByteString)
parseModule defs = Module <$> traverse (\(n, src) -> Fun n <$> runUntypedExpr src) defs

runMono :: [(ByteString, ByteString)] -> Either ByteString (Module (Expr (MType ByteString) ByteString) ByteString)
runMono defs = monomorphise =<< inferModule [] =<< parseModule defs

monosTo :: [(ByteString, ByteString)] -> [(ByteString, String)] -> Assertion
monosTo defs expected =
    case runMono defs of
        Left err -> assertFailure (C8.unpack err)
        Right md -> [ (n, renderType (unMono (mTypeOf e))) | Fun n e <- getFunDefns md ] @?= expected

-- | The top-level names that the named function's body refers to.
refersTo :: [(ByteString, ByteString)] -> ByteString -> [ByteString] -> Assertion
refersTo defs name expected =
    case runMono defs of
        Left err -> assertFailure (C8.unpack err)
        Right md ->
            let globals = S.fromList [ n | Fun n _ <- getFunDefns md ]
            in fmap (S.toList . S.intersection globals . freeVars)
                    (lookup name [ (n, e) | Fun n e <- getFunDefns md ])
                   @?= Just expected

unMono :: MType s -> Type s
unMono = \case
    MTyCon c    -> TyCon c
    MTyArr ps r -> TyArr (map unMono ps) (unMono r)
    MTyApp f x  -> TyApp (unMono f) (unMono x)
