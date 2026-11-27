module Main ( main ) where

import Parse.LexAndParse (runUntypedExpr)
import Pretty.Class      (pretty)

import qualified Data.ByteString.Char8 as C8
import           Data.Text                (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import           Network.Wai              (Middleware, requestHeaders)
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Middleware.Cors
import           Servant
import           Control.Applicative      ((<|>))
import           Control.Monad            (unless)
import           System.Directory         (doesFileExist, makeAbsolute)
import           System.Environment       (getArgs)
import           System.Exit              (die)
import           WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import           WaiAppStatic.Types       (ssIndices, unsafeToPiece)
import           Web.FormUrlEncoded       (FromForm (..), parseUnique)

newtype CompileReq =
    CompileReq Text

instance FromForm CompileReq where
    fromForm f = CompileReq <$> parseUnique "source" f

type Api = "compile" :> ReqBody '[FormUrlEncoded] CompileReq :> Post '[PlainText] Text
      :<|> Raw

server :: FilePath -> Server Api
server staticDir = compileH
              :<|> serveDirectoryWith (defaultWebAppSettings staticDir)
                       { ssIndices = [unsafeToPiece "index.html"] }

-- Errors are returned as 200 so htmx swaps them into the output pane.
compileH :: CompileReq -> Handler Text
compileH (CompileReq src) =
    pure . decode $
        case runUntypedExpr (T.encodeUtf8 src) of
            Left err   -> "error: " <> err
            Right expr -> pretty expr
  where
    decode = T.decodeUtf8With T.lenientDecode

-- Dev-only CORS: allow pages served from any localhost port, or opened from
-- file:// (Origin: null). htmx's HX-* request headers force a preflight.
devCors :: Middleware
devCors = cors $ \req -> do
    origin <- lookup "Origin" (requestHeaders req)
    if isLocal origin
        then Just simpleCorsResourcePolicy
                { corsOrigins        = Just ([origin], False)
                , corsRequestHeaders = [ "Content-Type", "HX-Request", "HX-Current-URL"
                                       , "HX-Target", "HX-Trigger", "HX-Trigger-Name"
                                       , "HX-Boosted", "HX-Prompt" ]
                , corsVaryOrigin     = True
                }
        else Nothing
  where
    isLocal "null" = True
    isLocal o      = C8.takeWhile (/= ':') (dropScheme o) `elem` ["localhost", "127.0.0.1", "[::1]"]
    dropScheme o   = maybe o id $  C8.stripPrefix "http://" o
                               <|> C8.stripPrefix "https://" o

-- Usage: compiler-server [PORT] [STATIC_DIR]
main :: IO ()
main = do
    args <- getArgs
    let (port, staticDir) =
            case args of
                [p, d] -> (read p, d)
                [p]    -> (read p, "frontend")
                _      -> (8080, "frontend")
    absDir <- makeAbsolute staticDir
    found  <- doesFileExist (absDir <> "/index.html")
    unless found . die $ "No index.html in " <> absDir <> " (run from the repo root, or pass STATIC_DIR)"
    putStrLn $ "Serving " <> absDir <> " at http://localhost:" <> show port <> "/"
    run port . devCors $ serve (Proxy :: Proxy Api) (server absDir)
