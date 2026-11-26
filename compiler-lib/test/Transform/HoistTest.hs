module Transform.HoistTest ( tests ) where

import Core.Expression
import Core.Module
import Parse.LexAndParse
import Transform.ClosureConvert
import Transform.Hoist

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor.Identity   (Identity (..))
import qualified Data.Set as S

import Test.Tasty       (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))

tests :: TestTree
tests = testGroup "Hoist"

    [ testCase "application without lambdas is unchanged" $
        hoistsTo "f (g x)"
            [ fun "main" [] [] (app (var "f") [app (var "g") [var "x"]]) ]

    , testCase "top-level lambda becomes the function's parameters" $
        hoistsTo "\\x. x"
            [ fun "main" [] ["x"] (var "x") ]

    , testCase "inner lambda is hoisted and replaced by a closure" $
        hoistsTo "\\x. \\y. x"
            [ fun "ll_0" ["x"] ["y"] (var "x")
            , fun "main" []    ["x"] (mkClo ["x"] "ll_0")
            ]

    , testCase "inner closure that captures nothing still gets a MkClo" $
        hoistsTo "\\x. \\y. y"
            [ fun "ll_0" [] ["y"] (var "y")
            , fun "main" [] ["x"] (mkClo [] "ll_0")
            ]

    , testCase "nested lambdas are hoisted innermost first" $
        hoistsTo "\\x. \\y. \\z. x y z"
            [ fun "ll_1" ["x", "y"] ["z"] (app (var "x") [var "y", var "z"])
            , fun "ll_0" ["x"]      ["y"] (mkClo ["x", "y"] "ll_1")
            , fun "main" []         ["x"] (mkClo ["x"] "ll_0")
            ]

    , testCase "lambda in argument position is hoisted" $
        hoistsTo "\\x. f (\\y. x)"
            [ fun "ll_0" ["x"] ["y"] (var "x")
            , fun "main" []    ["x"] (app (var "f") [mkClo ["x"] "ll_0"])
            ]

    , testCase "sibling lambdas get distinct names" $
        hoistsTo "\\x. f (\\y. x) (\\z. z)"
            [ fun "ll_0" ["x"] ["y"] (var "x")
            , fun "ll_1" []    ["z"] (var "z")
            , fun "main" []    ["x"]
                (app (var "f") [mkClo ["x"] "ll_0", mkClo [] "ll_1"])
            ]

    , testCase "closed let-bound function is lifted and its binding removed" $
        hoistsTo "\\x. let f y = y in \\z. f z"
            [ fun "ll_0" [] ["y"] (var "y")
            , fun "ll_1" [] ["z"] (app (var "ll_0") [var "z"])
            , fun "main" [] ["x"] (mkClo [] "ll_1")
            ]

    , testCase "recursive reference in a lifted function is renamed" $
        hoistsTo "let f x = f x in f 1"
            [ fun "ll_0" [] ["x"] (app (var "ll_0") [var "x"])
            , fun "main" [] []    (app (var "ll_0") [int 1])
            ]

    , testCase "let-bound function with free locals keeps its binding" $
        hoistsTo "\\x. let f y = x in \\z. f z"
            [ fun "ll_0" ["x"] ["y"] (var "x")
            , fun "ll_1" ["f"] ["z"] (app (var "f") [var "z"])
            , fun "main" []    ["x"]
                (lett [("f", mkClo ["x"] "ll_0")] (mkClo ["f"] "ll_1"))
            ]

    , testCase "let-bound value keeps its binding" $
        hoistsTo "\\x. let a = 1 in \\z. a"
            [ fun "ll_0" ["a"] ["z"] (var "a")
            , fun "main" []    ["x"] (lett [("a", int 1)] (mkClo ["a"] "ll_0"))
            ]

    , testCase "lambda parameter shadows a lifted name" $
        hoistsTo "let f x = x in \\f. f"
            [ fun "ll_0" [] ["x"] (var "x")
            , fun "ll_1" [] ["f"] (var "f")
            , fun "main" [] []    (mkClo [] "ll_1")
            ]

    , testCase "let binder shadows a lifted name" $
        hoistsTo "let f x = x in let f = 1 in f"
            [ fun "ll_0" [] ["x"] (var "x")
            , fun "main" [] []    (lett [("f", int 1)] (var "f"))
            ]

    , testCase "lifted name is not renamed outside its let" $
        hoistsTo "g (let f x = x in f) f"
            [ fun "ll_0" [] ["x"] (var "x")
            , fun "main" [] []    (app (var "g") [var "ll_0", var "f"])
            ]

    , testCase "globals are referenced directly" $
        hoistsModuleTo [("main", "\\x. \\y. g x"), ("g", "\\a. a")]
            [ fun "ll_0" ["x"] ["y"] (app (var "g") [var "x"])
            , fun "main" []    ["x"] (mkClo ["x"] "ll_0")
            , fun "g"    []    ["a"] (var "a")
            ]

    , testCase "fresh names are unique across the whole module" $
        hoistsModuleTo [("main", "\\x. \\y. x"), ("other", "\\a. \\b. a")]
            [ fun "ll_0"  ["x"] ["y"] (var "x")
            , fun "ll_1"  ["a"] ["b"] (var "a")
            , fun "main"  []    ["x"] (mkClo ["x"] "ll_0")
            , fun "other" []    ["a"] (mkClo ["a"] "ll_1")
            ]

    , testCase "mixed lifted and captured bindings" $
        hoistsTo "let f x = f x in let a = 1 in let g y = a in g f"
            [ fun "ll_0" []    ["x"] (app (var "ll_0") [var "x"])
            , fun "ll_1" ["a"] ["y"] (var "a")
            , fun "main" []    []
                (lett [("a", int 1)]
                    (lett [("g", mkClo ["a"] "ll_1")]
                        (app (var "g") [var "ll_0"])))
            ]
    ]

hoistsTo :: ByteString -> [HFun () ByteString] -> Assertion
hoistsTo src = hoistsModuleTo [("main", src)]

hoistsModuleTo :: [(ByteString, ByteString)] -> [HFun () ByteString] -> Assertion
hoistsModuleTo srcs expected =

    case traverse (\(n, src) -> Fun n <$> runUntypedExpr src) srcs of

        Left err ->
            assertFailure $ "parse error: " <> C8.unpack err

        Right parsed -> do
            let Identity cc               = closureConvert (Module parsed) :: Identity (Module (CCExpr () ByteString) ByteString)
                Identity (HModule actual) = hoist cc
            actual @?= expected

fun :: ByteString -> [ByteString] -> [ByteString] -> HExpr () ByteString -> HFun () ByteString
fun name env = HFun name (S.fromList env)

var :: ByteString -> HExpr () ByteString
var = HTerm . Var ()

int :: Int -> HExpr () ByteString
int = HTerm . LitInt

app :: HExpr () ByteString -> [HExpr () ByteString] -> HExpr () ByteString
app = HApp ()

mkClo :: [ByteString] -> ByteString -> HExpr () ByteString
mkClo env = HMkClo () (S.fromList env)

lett :: [(ByteString, HExpr () ByteString)] -> HExpr () ByteString -> HExpr () ByteString
lett bs = HLet [((), n, e) | (n, e) <- bs]
