module Parse.LexAndParse ( lexAndParseWith
                         , runModule
                         , runUntypedExpr
                         ) where

import Core.Expression
import Core.Module
import Parse.Expression
import Parse.Lex
import Parse.Module
import Parse.Parser
import Parse.Token

import           Data.ByteString             (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet as IS
import           Data.Vector                 (Vector)
import qualified Data.Vector as V
import           Data.Word                   (Word8)

runUntypedExpr :: ByteString -> Either ByteString (Expr () ByteString)
runUntypedExpr input = do
    (_tokens, eExpr)    <- lexAndParseWith parseExpr input
    (_parseState, expr) <- eExpr
    pure expr

runModule :: ByteString -> Either ByteString (Module (Expr () ByteString) ByteString)
runModule input = do
    (tokens, eModule) <- lexAndParseWith parseDefns input
    (parseState, md)  <- eModule
    if ps_pos parseState == length tokens
        then pure md
        else Left $ "Unparsed input from token " <> C8.pack (show (ps_pos parseState))
                 <> ": " <> C8.pack (show (tokens V.! ps_pos parseState))

lexAndParseWith :: Parser ParseState a
                -> ByteString
                -> Either ByteString (Vector Token, Either ByteString (ParseState, a))
lexAndParseWith p source = do
    let lineStarts = findLineStarts source
    (positions, tokens) <- runLexer source
    let pr = runParser p $ ParseState { ps_tokens     = tokens
                                      , ps_pos        = 0
                                      , ps_positions  = positions
                                      , ps_lineStarts = lineStarts
                                      }
    pure (tokens, pr)

data LineState =
    LineState { newLines  :: ![Int]
              , _position :: !Int
              , _last     :: !Word8 }

findLineStarts :: ByteString -> IntSet
findLineStarts bs
    | BS.null bs = mempty
    | otherwise  = IS.fromList . newLines . BS.foldl' f (LineState [0] 0 0) $ bs
    where
    f :: LineState -> Word8 -> LineState
    f (LineState nl p l) x =
        let nl' = case l of
                      10 -> p : nl
                      13 -> case x of
                                10 -> nl
                                _  -> p : nl
                      _  -> nl
        in LineState nl' (p+1) x
