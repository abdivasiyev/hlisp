{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HLisp.Parser (
    Expr (..),
    parseTokens,
    parseSrc,
) where

import Data.Set qualified as Set
import Data.Text (Text)
import Data.Void (Void)
import HLisp.Lexer qualified as L
import HLisp.Token qualified as T
import Text.Megaparsec

type Parser = Parsec Void [T.Token]

data Expr
    = ESymbol Text
    | ENumber Integer
    | EString Text
    | EBool Bool
    | EList [Expr]
    | EDotted [Expr] Expr
    | EUnquote Expr
    | EUnquoteSplicing Expr
    deriving (Eq, Show)

satisfyTok :: (T.Token -> Maybe a) -> Parser a
satisfyTok f = token f Set.empty

parseBool :: Parser Expr
parseBool = satisfyTok $ \case
    T.TokBool b -> Just (EBool b)
    _ -> Nothing

parseNumber :: Parser Expr
parseNumber = satisfyTok $ \case
    T.TokNumber n -> Just (ENumber n)
    _ -> Nothing

parseString :: Parser Expr
parseString = satisfyTok $ \case
    T.TokString s -> Just (EString s)
    _ -> Nothing

parseSymbol :: Parser Expr
parseSymbol = satisfyTok $ \case
    T.TokSymbol s -> Just (ESymbol s)
    _ -> Nothing

parseQuote :: Parser Expr
parseQuote = do
    _ <- satisfyTok $ \case
        T.TokQuote -> Just ()
        _ -> Nothing
    x <- parseExpr
    return $ EList [ESymbol "quote", x]

parseBackQuote :: Parser Expr
parseBackQuote = do
    _ <- satisfyTok $ \case
        T.TokBackQuote -> Just ()
        _ -> Nothing
    x <- parseExpr
    return $ EList [ESymbol "quasiquote", x]

parseUnquote :: Parser Expr
parseUnquote = do
    _ <- satisfyTok $ \case
        T.TokComma -> Just ()
        _ -> Nothing
    EUnquote <$> parseExpr

parseUnquoteSplicing :: Parser Expr
parseUnquoteSplicing = do
    _ <- satisfyTok $ \case
        T.TokCommaAt -> Just ()
        _ -> Nothing
    EUnquoteSplicing <$> parseExpr

parseDotted :: [Expr] -> Parser Expr
parseDotted xs = do
    _ <- satisfyTok $ \case
        T.TokDot -> Just ()
        _ -> Nothing
    rest <- parseExpr
    parseCloseParen

    return $ EDotted xs rest

parseOpenParen :: Parser ()
parseOpenParen = do
    _ <- satisfyTok $ \case
        T.TokLParen -> Just ()
        _ -> Nothing
    return ()

parseCloseParen :: Parser ()
parseCloseParen = do
    _ <- satisfyTok $ \case
        T.TokRParen -> Just ()
        _ -> Nothing
    return ()

parseList :: Parser Expr
parseList = do
    parseOpenParen
    xs <- many parseExpr
    parseDotted xs <|> (parseCloseParen >> return (EList xs))

parseExpr :: Parser Expr
parseExpr =
    choice
        [ parseBool
        , parseNumber
        , parseString
        , parseSymbol
        , parseQuote
        , parseBackQuote
        , parseUnquoteSplicing
        , parseUnquote
        , parseList
        ]

parseTokens :: [T.Token] -> Either (ParseErrorBundle [T.Token] Void) Expr
parseTokens = runParser (EList <$> (many parseExpr <* eof)) "<tokens>"

parseSrc :: Text -> Either String Expr
parseSrc input = case L.tokenize input of
    Left lexErr -> Left ("Lexing error:\n" <> errorBundlePretty lexErr)
    Right toks -> case parseTokens toks of
        -- TODO: enable pretty-printing of parse errors for parseErr
        Left parseErr -> Left ("Parsing error:\n" <> show parseErr)
        Right expr -> Right expr