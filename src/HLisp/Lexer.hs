{-# LANGUAGE OverloadedStrings #-}

module HLisp.Lexer (
    Parser,
    tokenize,
) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)

import Text.Megaparsec
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

import HLisp.Token qualified as Tok

type Parser = Parsec Void Text

-- | Space consumer: skips whitespace and comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment ";") empty

-- | Lexeme parser: applies a parser and then consumes trailing space
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser: parses a given string and consumes trailing space
symbol' :: Text -> Parser Text
symbol' = L.symbol sc

-- | Parses a left parenthesis '('
parseLParen :: Parser Tok.Token
parseLParen = Tok.TokLParen <$ symbol' "("

-- | Parses a right parenthesis ')'
parseRParen :: Parser Tok.Token
parseRParen = Tok.TokRParen <$ symbol' ")"

-- | Parses a quote '''
parseQuote :: Parser Tok.Token
parseQuote = Tok.TokQuote <$ symbol' "'"

-- | Parses a backquote '`'
parseBackQuote :: Parser Tok.Token
parseBackQuote = Tok.TokBackQuote <$ symbol' "`"

-- | Parses a comma ','
parseComma :: Parser Tok.Token
parseComma = Tok.TokComma <$ symbol' ","

-- | Parses a comma-at ',@'
parseCommaAt :: Parser Tok.Token
parseCommaAt = Tok.TokCommaAt <$ symbol' ",@"

-- | Parses a dot '.'
parseDot :: Parser Tok.Token
parseDot = Tok.TokDot <$ symbol' "."

-- | Parses a boolean '#t' or '#f'
parseBool :: Parser Tok.Token
parseBool = (Tok.TokBool True <$ symbol' "#t") <|> (Tok.TokBool False <$ symbol' "#f")

-- | Parses a number (integer)
parseNumber :: Parser Tok.Token
parseNumber = lexeme . try $ do
    sign <- optional (char '-')
    digits <- some digitChar
    let num = read digits :: Integer
    pure . Tok.TokNumber . maybe num (const (negate num)) $ sign

-- | Parses a string literal enclosed in double quotes
parseString :: Parser Tok.Token
parseString = Tok.TokString . T.pack <$> (char '"' *> manyTill L.charLiteral (char '"'))

-- | Parses a symbol (identifier)
parseSymbol :: Parser Tok.Token
parseSymbol = Tok.TokSymbol . T.pack <$> lexeme ((:) <$> initial <*> many subsequent)
  where
    initial = letterChar <|> oneOf ("!$%&*/:<=>?^_~+-" :: String)
    subsequent = initial <|> digitChar <|> oneOf (".@" :: String)

-- | Parses a single token
parseToken :: Parser Tok.Token
parseToken =
    choice
        [ parseLParen
        , parseRParen
        , parseQuote
        , parseBackQuote
        , parseCommaAt
        , parseComma
        , parseDot
        , parseBool
        , parseNumber
        , parseString
        , parseSymbol
        ]

-- | Parses a list of tokens from the input text
tokenize :: Text -> Either (ParseErrorBundle Text Void) [Tok.Token]
tokenize = MP.runParser (sc *> many (lexeme parseToken) <* eof) "<stdin>"