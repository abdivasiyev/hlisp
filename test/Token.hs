{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Token (
    ppToken,
) where

import Data.Text (Text)
import Data.Text qualified as T
import HLisp.Token (Token (..))
import Test.Tasty.QuickCheck

arbitrarySymbol :: Gen Token
arbitrarySymbol = do
    firstCh <- elements initialChars
    rest <- listOf (elements subsequentChars)

    pure . TokSymbol . T.pack $ firstCh : rest
  where
    initialChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!$%&*/:<=>?^_~"
    subsequentChars = initialChars ++ ['0' .. '9'] ++ ".@"

-- | Quickcheck Arbitrary instance for Token
instance Arbitrary Token where
    arbitrary =
        oneof
            [ pure TokLParen
            , pure TokRParen
            , pure TokQuote
            , pure TokBackQuote
            , pure TokComma
            , pure TokCommaAt
            , pure TokDot
            , arbitrarySymbol
            , TokNumber <$> arbitrary
            , TokString . T.pack <$> listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ " !#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))
            , TokBool <$> arbitrary
            ]
    shrink = \case
        (TokNumber n) -> TokNumber <$> shrink n
        (TokString s) -> TokString . T.pack <$> (shrink . T.unpack $ s)
        (TokSymbol s) -> [TokSymbol (T.pack t) | t <- shrink (T.unpack s), not (null t)]
        (TokBool b) -> TokBool <$> shrink b
        _ -> []

ppToken :: Token -> Text
ppToken = \case
    TokLParen -> "("
    TokRParen -> ")"
    TokQuote -> "'"
    TokBackQuote -> "`"
    TokComma -> ","
    TokCommaAt -> ",@"
    TokDot -> "."
    TokSymbol s -> s
    TokNumber n -> T.pack (show n)
    TokString s -> "\"" <> escape s <> "\""
    TokBool True -> "#t"
    TokBool False -> "#f"
  where
    escape = T.concatMap f
    f '\"' = "\\\""
    f '\\' = "\\\\"
    f c = T.singleton c