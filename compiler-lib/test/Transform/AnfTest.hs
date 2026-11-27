module Transform.AnfTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Transform.Anf
import Transform.ClosureConvert
import Transform.Hoist

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor.Identity   (Identity (..))
import qualified Data.Set as S

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "Anf"

    [ testCase "atom is unchanged" $
        normsTo "\\x. x"
            [ fun "main" [] ["x"] (atom (var "x")) ]

    , testCase "application of atoms is unchanged" $
        normsTo "f x 1"
            [ fun "main" [] [] (capp (var "f") [var "x", int 1]) ]

    , testCase "nested application is let-bound" $
        normsTo "f (g x)"
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "g") [var "x"])
                    (capp (var "f") [var "anf_0"]))
            ]

    , testCase "arguments are bound left to right" $
        normsTo "f (g a) (h b)"
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "g") [var "a"])
                    (nlet "anf_1" (capp (var "h") [var "b"])
                        (capp (var "f") [var "anf_0", var "anf_1"])))
            ]

    , testCase "complex function position is let-bound" $
        normsTo "(f a) b"
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "f") [var "a"])
                    (capp (var "anf_0") [var "b"]))
            ]

    , testCase "closure argument is let-bound" $
        normsTo "\\x. f (\\y. x)"
            [ fun "ll_0" ["x"] ["y"] (atom (var "x"))
            , fun "main" []    ["x"]
                (nlet "anf_0" (cmkClo ["x"] "ll_0")
                    (capp (var "f") [var "anf_0"]))
            ]

    , testCase "let keeps its name" $
        normsTo "let a = g 1 in f a"
            [ fun "main" [] []
                (nlet "a" (capp (var "g") [int 1])
                    (capp (var "f") [var "a"]))
            ]

    , testCase "let in argument position is floated out" $
        normsTo "f (let a = g 1 in a)"
            [ fun "main" [] []
                (nlet "a" (capp (var "g") [int 1])
                    (capp (var "f") [var "a"]))
            ]

    , testCase "floated let that would capture a parameter is renamed" $
        normsTo "\\x. f (let x = 1 in x) x"
            [ fun "main" [] ["x"]
                (nlet "anf_0" (atom (int 1))
                    (capp (var "f") [var "anf_0", var "x"]))
            ]

    , testCase "floated let that would capture an outer let is renamed" $
        normsTo "let a = 1 in f (let a = 2 in a) a"
            [ fun "main" [] []
                (nlet "a" (atom (int 1))
                    (nlet "anf_0" (atom (int 2))
                        (capp (var "f") [var "anf_0", var "a"])))
            ]

    , testCase "floated let that would capture a global is renamed" $
        normsModuleTo [("main", "f (let g = 1 in g) g"), ("g", "\\a. a")]
            [ fun "main" [] []
                (nlet "anf_0" (atom (int 1))
                    (capp (var "f") [var "anf_0", var "g"]))
            , fun "g" [] ["a"] (atom (var "a"))
            ]

    , testCase "renamed let is renamed in closure captures" $
        normsTo "\\x. f (let x = 1 in \\y. x) x"
            [ fun "ll_0" ["x"] ["y"] (atom (var "x"))
            , fun "main" []    ["x"]
                (nlet "anf_0" (atom (int 1))
                    (nlet "anf_1" (cmkClo ["anf_0"] "ll_0")
                        (capp (var "f") [var "anf_1", var "x"])))
            ]

    , testCase "recursive let is a NLetRec" $
        normsTo "\\x. let f y = f x in f"
            [ fun "ll_0" ["f", "x"] ["y"] (capp (var "f") [var "x"])
            , fun "main" []         ["x"]
                (nletRec [("f", cmkClo ["f", "x"] "ll_0")] (atom (var "f")))
            ]

    , testCase "recursive let's right-hand side is normalised in place" $
        normsTo "\\x. let f = g (\\y. f y) in f"
            [ fun "ll_0" ["f"] ["y"] (capp (var "f") [var "y"])
            , fun "main" []    ["x"]
                (nletRec [("f", nlet "anf_0" (cmkClo ["f"] "ll_0")
                                    (capp (var "g") [var "anf_0"]))]
                    (atom (var "f")))
            ]

    , testCase "fresh names are unique across the whole module" $
        normsModuleTo [("main", "f (g x)"), ("other", "h (k y)")]
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "g") [var "x"])
                    (capp (var "f") [var "anf_0"]))
            , fun "other" [] []
                (nlet "anf_1" (capp (var "k") [var "y"])
                    (capp (var "h") [var "anf_1"]))
            ]

    , testCase "constructor fields are let-bound" $
        normsExprTo
            (DataCon () "Cons" [App () (eVar "g") [eVar "x"], DataCon () "Nil" []])
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "g") [var "x"])
                    (nlet "anf_1" (cdataCon "Nil" [])
                        (cdataCon "Cons" [var "anf_0", var "anf_1"])))
            ]

    , testCase "complex scrutinee is let-bound" $
        normsExprTo
            (Case () (App () (eVar "f") [eVar "x"]) [ (PWild, Term (LitInt 1)) ])
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "f") [var "x"])
                    (ccase (var "anf_0") [ (PWild, atom (int 1)) ]))
            ]

    , testCase "case in argument position is let-bound" $
        normsExprTo
            (App () (eVar "f") [Case () (eVar "x") [ (PWild, Term (LitInt 1)) ]])
            [ fun "main" [] []
                (nlet "anf_0" (ccase (var "x") [ (PWild, atom (int 1)) ])
                    (capp (var "f") [var "anf_0"]))
            ]

    , testCase "lets in an alternative stay in the alternative" $
        normsExprTo
            (Case () (eVar "x")
                [ (PDataCon "Nil" [],
                   App () (eVar "f") [Let [((), "a", App () (eVar "g") [Term (LitInt 1)])] (eVar "a")]) ])
            [ fun "main" [] []
                (ccase (var "x")
                    [ (PDataCon "Nil" [],
                       nlet "a" (capp (var "g") [int 1]) (capp (var "f") [var "a"])) ])
            ]

    , testCase "binop of atoms is unchanged" $
        normsExprTo
            (BinOp () Plus (eVar "x") (Term (LitInt 1)))
            [ fun "main" [] [] (cbinOp Plus (var "x") (int 1)) ]

    , testCase "binop operands are let-bound left to right" $
        normsExprTo
            (BinOp () Times (App () (eVar "f") [eVar "a"]) (App () (eVar "g") [eVar "b"]))
            [ fun "main" [] []
                (nlet "anf_0" (capp (var "f") [var "a"])
                    (nlet "anf_1" (capp (var "g") [var "b"])
                        (cbinOp Times (var "anf_0") (var "anf_1"))))
            ]

    , testCase "nested binop is let-bound" $
        normsExprTo
            (App () (eVar "f") [BinOp () Plus (eVar "x") (BinOp () Times (eVar "y") (eVar "z"))])
            [ fun "main" [] []
                (nlet "anf_0" (cbinOp Times (var "y") (var "z"))
                    (nlet "anf_1" (cbinOp Plus (var "x") (var "anf_0"))
                        (capp (var "f") [var "anf_1"])))
            ]

    , testCase "binop operand inside a lambda is captured" $
        normsExprTo
            (Lam () ["x"] (Lam () ["y"] (BinOp () Plus (eVar "x") (eVar "y"))))
            [ fun "ll_0" ["x"] ["y"] (cbinOp Plus (var "x") (var "y"))
            , fun "main" []    ["x"] (cmkClo ["x"] "ll_0")
            ]

    , testCase "pattern variable that shadows a parameter is renamed" $
        normsExprTo
            (Lam () ["x"] (Case () (eVar "x")
                [ (PDataCon "Cons" [PVar "x", PVar "t"], App () (eVar "f") [eVar "x", eVar "t"]) ]))
            [ fun "main" [] ["x"]
                (ccase (var "x")
                    [ (PDataCon "Cons" [PVar "anf_0", PVar "t"],
                       capp (var "f") [var "anf_0", var "t"]) ])
            ]

    , testCase "floated let that would capture a pattern variable is renamed" $
        normsExprTo
            (Case () (eVar "x")
                [ (PDataCon "Cons" [PVar "y", PWild],
                   App () (eVar "f") [Let [((), "y", Term (LitInt 1))] (eVar "y"), eVar "y"]) ])
            [ fun "main" [] []
                (ccase (var "x")
                    [ (PDataCon "Cons" [PVar "y", PWild],
                       nlet "anf_0" (atom (int 1)) (capp (var "f") [var "anf_0", var "y"])) ])
            ]

    , testCase "constructor mentioning its own let is a NLetRec" $
        normsExprTo
            (Let [((), "xs", DataCon () "Cons" [Term (LitInt 1), eVar "xs"])] (eVar "xs"))
            [ fun "main" [] []
                (nletRec [("xs", cdataCon "Cons" [int 1, var "xs"])] (atom (var "xs")))
            ]

    , testCase "a pattern-bound name doesn't make a let recursive" $
        normsExprTo
            (Let [((), "a", Case () (eVar "x") [ (PDataCon "Cons" [PVar "a", PWild], eVar "a") ])]
                 (eVar "a"))
            [ fun "main" [] []
                (nlet "anf_0" (ccase (var "x") [ (PDataCon "Cons" [PVar "a", PWild], atom (var "a")) ])
                    (atom (var "anf_0")))
            ]
    ]

normsTo :: ByteString -> [NFun () ByteString] -> Assertion
normsTo src = normsModuleTo [("main", src)]

normsModuleTo :: [(ByteString, ByteString)] -> [NFun () ByteString] -> Assertion
normsModuleTo srcs expected =

    case traverse (\(n, src) -> Fun n <$> runUntypedExpr src) srcs of

        Left err ->
            assertFailure $ "parse error: " <> C8.unpack err

        Right parsed ->
            normsParsedTo parsed expected

normsExprTo :: Expr () ByteString -> [NFun () ByteString] -> Assertion
normsExprTo e = normsParsedTo [Fun "main" e]

normsParsedTo :: [Fun (Expr () ByteString) ByteString] -> [NFun () ByteString] -> Assertion
normsParsedTo parsed expected = do
    let Identity cc               = closureConvert (Module parsed) :: Identity (Module (CCExpr () ByteString) ByteString)
        Identity hoisted          = hoist cc
        Identity (NModule actual) = anf hoisted
    actual @?= expected

eVar :: ByteString -> Expr () ByteString
eVar = Term . Var ()

fun :: ByteString -> [ByteString] -> [ByteString] -> NExpr () ByteString -> NFun () ByteString
fun name env = NFun name (S.fromList env)

var :: ByteString -> Term () ByteString
var = Var ()

int :: Int -> Term () ByteString
int = LitInt

atom :: Term () ByteString -> NExpr () ByteString
atom = AExpr

capp :: Term () ByteString -> [Term () ByteString] -> NExpr () ByteString
capp f = CExpr . CApp () f

cmkClo :: [ByteString] -> ByteString -> NExpr () ByteString
cmkClo env = CExpr . CMkClo () (S.fromList env)

cbinOp :: Op -> Term () ByteString -> Term () ByteString -> NExpr () ByteString
cbinOp op a = CExpr . CBinOp () op a

cdataCon :: ByteString -> [Term () ByteString] -> NExpr () ByteString
cdataCon c = CExpr . CDataCon () c

ccase :: Term () ByteString -> [(Pattern ByteString, NExpr () ByteString)] -> NExpr () ByteString
ccase scrut = CExpr . CCase () scrut

nlet :: ByteString -> NExpr () ByteString -> NExpr () ByteString -> NExpr () ByteString
nlet = NLet ()

nletRec :: [(ByteString, NExpr () ByteString)] -> NExpr () ByteString -> NExpr () ByteString
nletRec bs = NLetRec [((), n, e) | (n, e) <- bs]
