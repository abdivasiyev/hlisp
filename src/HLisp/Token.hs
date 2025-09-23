module HLisp.Token (
    Token (..),
) where

import Data.Text (Text)

data Token
    = TokLParen -- ( left parenthesis
    | TokRParen -- ) right parenthesis
    | TokQuote -- ' quote ; shorthand for (quote ...)
    | TokBackQuote -- ` backquote ; shorthand for quasiquote
    | TokComma -- , comma ; shorthand for unquote
    | TokCommaAt -- ,@ comma-at ; shorthand for unquote-splicing
    | TokDot -- . dot ; used in dotted pairs
    | TokSymbol Text -- identifiers foo, bar, +, -, etc.
    | TokNumber Integer -- numeric literals
    | TokString Text -- string literals "hello"
    | TokBool Bool -- boolean literals #t and #f
    deriving (Eq, Show)