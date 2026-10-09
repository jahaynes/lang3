module Parse.LexAndParse ( runUntypedExpr
                         ) where

import Core.Expression
import Parse.Expression
import Parse.Lex
import Parse.Parser
import Parse.Token

import           Data.ByteString             (ByteString)
import qualified Data.ByteString as BS
import           Data.IntSet                 (IntSet)
import qualified Data.IntSet as IS
import           Data.Vector                 (Vector)
import           Data.Word                   (Word8)

runUntypedExpr :: ByteString -> Either ByteString (Expr () ByteString)
runUntypedExpr input = do
    (_tokens, eExpr)    <- lexAndParseWith parseExpr input
    (_parseState, expr) <- eExpr
    pure expr

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
