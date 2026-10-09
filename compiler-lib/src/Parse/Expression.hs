module Parse.Expression ( parseExpr
                        ) where

import Core.Expression
import Parse.Parser
import Parse.Token

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as C8
import           Data.Functor ((<&>))
import           Data.Vector           ((!?))

parseExpr :: Parser ParseState (Expr () ByteString)
parseExpr = parseApply

parseApply :: Parser ParseState (Expr () ByteString)
parseApply = parseApp

    where
    parseApp =
        parseWhileColumns1 MoreRight parseNonApply <&>
            \(f, xs) ->
                if null xs
                    then f
                    else App () f xs

parseNonApply :: Parser ParseState (Expr () ByteString)
parseNonApply = parseLet
            <|> parseLambda
            <|> parseTerm
            <|> parseParen

parseLet :: Parser ParseState (Expr () ByteString)
parseLet = do
    (f,xs) <- token TLet *> parseWhileColumns1 MoreRight parseLowerStart
    e1     <- token TEq  *> parseExpr
    e2     <- token TIn  *> parseExpr
    pure $ case xs of
        [] -> Let () f                  e1  e2
        _  -> Let () f (Lam () xs e1) e2

parseLambda :: Parser ParseState (Expr () ByteString)
parseLambda = do
    (v, vs) <- token TLambda *> parseWhileColumns1 NotLeft parseLowerStart
    body    <- token TDot    *> parseExpr
    pure $ Lam () (v:vs) body

parseTerm :: Parser ParseState (Expr () ByteString)
parseTerm = parseLiteral <|> parseVariable

parseLiteral :: Parser ParseState (Expr () ByteString)
parseLiteral = Term <$> parseLitString
                    <|> parseLitBool
                    <|> parseLitInt

    where
    parseLitString :: Parser ParseState (Term () ByteString)
    parseLitString = parseSatisfy "string" f
        where
        f (TLitString s) = Just (LitString s)
        f _              = Nothing

    parseLitBool :: Parser ParseState (Term () ByteString)
    parseLitBool = parseSatisfy "boolean" f
        where
        f (TLitBool b) = Just (LitBool b)
        f _            = Nothing

    parseLitInt :: Parser ParseState (Term () ByteString)
    parseLitInt = pos <|> neg

        where
        pos = parseSatisfy "integer" isInt
        neg = do
            parseNegate
            ji <- parseSatisfy "integer" isInt
            case ji of
                LitInt i -> pure $ LitInt (-i)
                _        -> error "isInt returned non-int"

        isInt (TLitInt i) = Just (LitInt $ fromIntegral i)  -- TODO - right place for cast?
        isInt           _ = Nothing

        parseNegate :: Parser ParseState ()
        parseNegate = parseSatisfy "negate" f
            where
            f TNegate = Just ()
            f       _ = Nothing

parseVariable :: Parser ParseState (Expr () ByteString)
parseVariable = Term . Var () <$> parseLowerStart

parseParen :: Parser ParseState (Expr () ByteString)
parseParen = token TLParen *> parseExpr <* token TRParen

parseLowerStart :: Parser ParseState ByteString
parseLowerStart = parseSatisfy "lowerStart" f
    where
    f (TLowerStart x) = Just x
    f               _ = Nothing

parseSatisfy :: ByteString
             -> (Token -> Maybe a)
             -> Parser ParseState a
parseSatisfy n p = Parser f
    where
    f ps@(ParseState tokens pos _ _)

        | pos == length tokens =
            Left $ "no more tokens for " <> n

        | otherwise =
            case tokens !? pos of
                Nothing -> Left "Failed index"
                Just t  ->
                    case p t of
                        Nothing -> Left $ "Not a " <> n
                        Just r  -> Right (ps {ps_pos = pos + 1}, r)

token :: Token -> Parser ParseState ()
token tok =
    parseSatisfy (C8.pack $ show tok) $ \t ->
        if t == tok
            then Just ()
            else Nothing
