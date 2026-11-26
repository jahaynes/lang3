module Transform.ClosureConvertTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Transform.ClosureConvert

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor.Identity   (Identity (..))
import qualified Data.Set as S

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "ClosureConvert"

    [ testCase "application without lambdas is unchanged" $
        convertsTo [] "f (g x)" $
            app (var "f") [app (var "g") [var "x"]]

    , testCase "top-level lambda captures nothing" $
        convertsTo [] "\\x. x" $
            clo [] ["x"] (var "x")

    , testCase "inner lambda captures outer parameter" $
        convertsTo [] "\\x. \\y. x" $
            clo [] ["x"] (clo ["x"] ["y"] (var "x"))

    , testCase "inner lambda captures only the outer parameters it uses" $
        convertsTo [] "\\a b c. \\y. c a" $
            clo [] ["a", "b", "c"] (clo ["a", "c"] ["y"] (app (var "c") [var "a"]))

    , testCase "shadowed parameter is not captured" $
        convertsTo [] "\\x. \\x. x" $
            clo [] ["x"] (clo [] ["x"] (var "x"))

    , testCase "globals are not captured" $
        convertsTo ["g"] "\\x. \\y. g x" $
            clo [] ["x"] (clo ["x"] ["y"] (app (var "g") [var "x"]))

    , testCase "unbound free variables are not captured" $
        convertsTo [] "\\x. \\y. h y" $
            clo [] ["x"] (clo [] ["y"] (app (var "h") [var "y"]))

    , testCase "captures from several levels up" $
        convertsTo [] "\\x. \\y. \\z. x y z" $
            clo [] ["x"]
                (clo ["x"] ["y"]
                    (clo ["x", "y"] ["z"] (app (var "x") [var "y", var "z"])))

    , testCase "lambda in argument position captures" $
        convertsTo [] "\\x. f (\\y. x)" $
            clo [] ["x"] (app (var "f") [clo ["x"] ["y"] (var "x")])

    , testCase "closed let-bound function is lifted, not captured" $
        convertsTo [] "\\x. let f y = y in \\z. f z" $
            clo [] ["x"]
                (lett [("f", clo [] ["y"] (var "y"))]
                      (clo [] ["z"] (app (var "f") [var "z"])))

    , testCase "let-bound function with free locals is captured" $
        convertsTo [] "\\x. let f y = x in \\z. f z" $
            clo [] ["x"]
                (lett [("f", clo ["x"] ["y"] (var "x"))]
                      (clo ["f"] ["z"] (app (var "f") [var "z"])))

    , testCase "let-bound value is captured" $
        convertsTo [] "\\x. let a = 1 in \\z. a" $
            clo [] ["x"]
                (lett [("a", int 1)]
                      (clo ["a"] ["z"] (var "a")))

    , testCase "lambda parameter shadows a global" $
        convertsTo ["g"] "\\g. \\y. g" $
            clo [] ["g"] (clo ["g"] ["y"] (var "g"))

    , testCase "constructor fields are converted" $
        convertsExprTo []
            (eLam ["x"] (DataCon () "Cons" [eLam ["y"] (eVar "x"), DataCon () "Nil" []])) $
            clo [] ["x"] (dataCon "Cons" [clo ["x"] ["y"] (var "x"), dataCon "Nil" []])

    , testCase "lambda in a case alternative captures pattern variables" $
        convertsExprTo []
            (eLam ["x"] (Case () (eVar "x")
                [ (PDataCon "Cons" [PVar "h", PVar "t"], eLam ["y"] (eVar "h"))
                , (PWild, eLam ["y"] (eVar "x")) ])) $
            clo [] ["x"] (caseOf (var "x")
                [ (PDataCon "Cons" [PVar "h", PVar "t"], clo ["h"] ["y"] (var "h"))
                , (PWild, clo ["x"] ["y"] (var "x")) ])

    , testCase "pattern variable shadows a global" $
        convertsExprTo ["g"]
            (eLam ["x"] (Case () (eVar "x")
                [ (PDataCon "Cons" [PVar "g", PWild], eLam ["y"] (eVar "g")) ])) $
            clo [] ["x"] (caseOf (var "x")
                [ (PDataCon "Cons" [PVar "g", PWild], clo ["g"] ["y"] (var "g")) ])

    , testCase "pattern variables are not free, so a let can still be lifted" $
        convertsExprTo []
            (eLam ["x"] (Let [((), "f", eLam ["y"] (Case () (eVar "y")
                                  [ (PDataCon "Cons" [PVar "h", PWild], eVar "h") ]))]
                             (eLam ["z"] (App () (eVar "f") [eVar "z"])))) $
            clo [] ["x"]
                (lett [("f", clo [] ["y"] (caseOf (var "y")
                                [ (PDataCon "Cons" [PVar "h", PWild], var "h") ]))]
                      (clo [] ["z"] (app (var "f") [var "z"])))
    ]

convertsTo :: [ByteString] -> ByteString -> CCExpr () ByteString -> Assertion
convertsTo globals src expected =

    case runUntypedExpr src of

        Left err ->
            assertFailure $ "parse error: " <> C8.unpack err

        Right parsed ->
            convertsExprTo globals parsed expected

convertsExprTo :: [ByteString] -> Expr () ByteString -> CCExpr () ByteString -> Assertion
convertsExprTo globals parsed expected = do
    let others       = [Fun g (Term (LitInt 0)) | g <- globals]
        Identity md' = closureConvert (Module (Fun "main" parsed : others))
    case getFunDefns md' of
        Fun _ actual : _ -> actual @?= expected
        []               -> assertFailure "no functions in output"

eVar :: ByteString -> Expr () ByteString
eVar = Term . Var ()

eLam :: [ByteString] -> Expr () ByteString -> Expr () ByteString
eLam = Lam ()

var :: ByteString -> CCExpr () ByteString
var = CCTerm . Var ()

int :: Int -> CCExpr () ByteString
int = CCTerm . LitInt

app :: CCExpr () ByteString -> [CCExpr () ByteString] -> CCExpr () ByteString
app = CCApp ()

clo :: [ByteString] -> [ByteString] -> CCExpr () ByteString -> CCExpr () ByteString
clo env = CCClosure () (S.fromList env)

dataCon :: ByteString -> [CCExpr () ByteString] -> CCExpr () ByteString
dataCon = CCDataCon ()

caseOf :: CCExpr () ByteString -> [(Pattern ByteString, CCExpr () ByteString)] -> CCExpr () ByteString
caseOf = CCCase ()

lett :: [(ByteString, CCExpr () ByteString)] -> CCExpr () ByteString -> CCExpr () ByteString
lett bs = CCLet [((), n, e) | (n, e) <- bs]
