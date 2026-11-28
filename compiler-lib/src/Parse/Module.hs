module Parse.Module ( parseDataDefn
                    , parseDefns
                    , parseFunDefn
                    , parseTypeSig
                    ) where

import Core.Expression
import Core.Module
import Parse.Expression
import Parse.Parser
import Parse.Token
import TypeSystem.Types

import           Data.ByteString (ByteString)
import qualified Data.IntSet as IS
import qualified Data.Vector as V
import           Data.Vector     ((!?))

data ModuleElement s = ModuleDataDefn (DataDefn s)
                     | ModuleTypeSig (TypeSig s)
                     | ModuleFunDefn (Fun (Expr () s) s)

parseDefns :: Parser ParseState (Module (Expr () ByteString) ByteString)
parseDefns = go [] [] [] . V.toList <$> many parseDefn

    where
    go dds tss fds                      [] = Module (reverse dds) (reverse tss) (reverse fds)
    go dds tss fds (ModuleDataDefn dd:mes) = go (dd:dds) tss fds mes
    go dds tss fds (ModuleTypeSig ts:mes)  = go dds (ts:tss) fds mes
    go dds tss fds (ModuleFunDefn fd:mes)  = go dds tss (fd:fds) mes

parseDefn :: Parser ParseState (ModuleElement ByteString)
parseDefn = atLineStart $ (ModuleDataDefn <$> parseDataDefn)
                      <|> (ModuleTypeSig  <$> parseTypeSig)
                      <|> (ModuleFunDefn  <$> parseFunDefn)

parseFunDefn :: Parser ParseState (Fun (Expr () ByteString) ByteString)
parseFunDefn = do
    (name, vars) <- parseWhileColumns1 MoreRight parseLowerStart
    token TEq
    expr <- parseExpr
    pure $ Fun name $
        case vars of
            [] -> expr
            _  -> Lam () vars expr

parseDataDefn :: Parser ParseState (DataDefn ByteString)
parseDataDefn = do
    name   <- atLineStart parseUpperStart
    tyVars <- notAtLineStarts parseLowerStart
    _      <- notAtLineStart (token TEq)
    dc1    <- notAtLineStart parseDataConstructor
    dcs    <- V.toList <$> many (notAtLineStart (token TPipe) *> notAtLineStart parseDataConstructor)
    pure $ DataDefn name tyVars (dc1:dcs)

    where
    parseDataConstructor :: Parser ParseState (DCon ByteString)
    parseDataConstructor = DCon <$> parseUpperStart
                                <*> notAtLineStarts parseAtomicType

parseTypeSig :: Parser ParseState (TypeSig ByteString)
parseTypeSig = do
    name <- atLineStart parseLowerStart
    _    <- notAtLineStart (token TColon)
    TypeSig name <$> parseType

parseType :: Parser ParseState (Type ByteString)
parseType = do
    t  <- parseAppliedType
    ts <- V.toList <$> many (notAtLineStart (token TArr) *> parseAppliedType)
    pure $ case ts of
        [] -> t
        _  -> TyArr (t : init ts) (last ts)

parseAppliedType :: Parser ParseState (Type ByteString)
parseAppliedType = do
    t  <- parseAtomicType
    ts <- notAtLineStarts parseAtomicType
    pure $ foldl TyApp t ts

parseAtomicType :: Parser ParseState (Type ByteString)
parseAtomicType = notAtLineStart $
        (TyVar <$> parseLowerStart)
    <|> (TyCon <$> parseUpperStart)
    <|> (token TLParen *> parseType <* token TRParen)

atLineStart :: Parser ParseState a -> Parser ParseState a
atLineStart p = Parser $ \ps ->
    if isAtLineStart ps
        then runParser p ps
        else Left "Not at line start"

notAtLineStart :: Parser ParseState a -> Parser ParseState a
notAtLineStart p = Parser $ \ps ->
    if isAtLineStart ps
        then Left "At line start"
        else runParser p ps

notAtLineStarts :: Parser ParseState a -> Parser ParseState [a]
notAtLineStarts p = V.toList <$> many (notAtLineStart p)

isAtLineStart :: ParseState -> Bool
isAtLineStart ps =
    case ps_positions ps !? ps_pos ps of
        Nothing -> False
        Just x  -> x `IS.member` ps_lineStarts ps
