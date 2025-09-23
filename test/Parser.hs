{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Parser (
    tests,
) where

import Data.Text qualified as T
import HLisp.Lexer
import HLisp.Parser

import Test.Tasty
import Test.Tasty.QuickCheck

instance Arbitrary Expr where
    arbitrary = sized genExpr
    shrink = shrinkExpr

shrinkExpr :: Expr -> [Expr]
shrinkExpr = \case
    ENumber n -> ENumber <$> shrink n
    EString s -> EString . T.pack <$> shrink (T.unpack s)
    EBool b -> EBool <$> shrink b
    EList [ESymbol "quote", x] -> [x]
    EList [ESymbol "quasiquote", x] -> [x]
    ESymbol s ->
        [ ESymbol (T.pack (h : rest'))
        | let str = T.unpack s
        , not (null str)
        , let h = head str
        , rest' <- shrink (tail str)
        , all (`elem` subsequentChars) rest'
        ]
    EList xs -> xs <> [EList ys | ys <- shrink xs]
    EDotted xs x ->
        xs
            <> [x]
            <> [ EDotted ys y
               | (ys, y) <-
                    shrink (xs <> [x]) >>= \ys -> case reverse ys of
                        (h : t) -> [(reverse t, h)]
                        [] -> []
               ]
    EUnquote x -> x : [EUnquote y | y <- shrinkExpr x]
    EUnquoteSplicing x -> x : [EUnquoteSplicing y | y <- shrinkExpr x]

initialChars, subsequentChars :: [Char]
initialChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ "!$%&*/:<=>?^_~+-"
subsequentChars = initialChars ++ ['0' .. '9'] ++ ".@"

genSymbol :: Gen T.Text
genSymbol = do
    first <- elements initialChars
    if first `elem` ['a' .. 'z'] ++ ['A' .. 'Z']
        then do
            rest <- listOf (elements subsequentChars)
            pure (T.pack (first : rest))
        else pure (T.singleton first)

genExpr :: Int -> Gen Expr
genExpr 0 =
    oneof
        [ ENumber <$> arbitrary
        , EString . T.pack <$> listOf (elements (['a' .. 'z'] ++ ['A' .. 'Z'] ++ " "))
        , EBool <$> arbitrary
        , ESymbol <$> genSymbol
        ]
genExpr n =
    oneof
        [ genExpr 0
        , EList <$> resize (n `div` 2) (listOf (genExpr (n `div` 2)))
        , do
            xs <- resize (n `div` 2) (listOf1 (genExpr (n `div` 2)))
            y <- genExpr (n `div` 2)
            pure (EDotted xs y)
        , EUnquote <$> genExpr (n - 1)
        , EUnquoteSplicing <$> genExpr (n - 1)
        , do
            x <- genExpr (n - 1)
            pure (EList [ESymbol "quote", x])
        , do
            x <- genExpr (n - 1)
            pure (EList [ESymbol "quasiquote", x])
        ]

ppExpr :: Expr -> T.Text
ppExpr = \case
    ENumber n -> T.pack (show n)
    EString s -> "\"" <> escape s <> "\""
    EBool True -> "#t"
    EBool False -> "#f"
    ESymbol s -> s
    EList xs -> "(" <> T.unwords (map ppExpr xs) <> ")"
    EDotted xs x -> "(" <> T.unwords (map ppExpr xs) <> " . " <> ppExpr x <> ")"
    EUnquote x -> "," <> ppExpr x
    EUnquoteSplicing x -> ",@" <> ppExpr x
  where
    escape = T.concatMap f
    f '"' = "\\\""
    f '\\' = "\\\\"
    f c = T.singleton c

prop_roundTrip :: Expr -> Property
prop_roundTrip expr =
    let src = ppExpr expr
     in case tokenize src of
            Left err -> counterexample ("Tokenization failed: " <> show err) False
            Right tokens ->
                case parseTokens tokens of
                    Left err -> counterexample ("Parsing failed: " <> show err <> "\nSource: " <> T.unpack src <> "\nTokens: " <> show tokens) False
                    Right (EList expr') ->
                        counterexample ("Parsed expression does not match original.\nSource: " <> show expr <> "\nOriginal: " <> T.unpack (ppExpr expr) <> "\nParsed: " <> T.unpack (ppExpr . head $ expr')) $
                            expr === head expr'
                    Right _ ->
                        counterexample ("Parsed expression is not a list.\nSource: " <> T.unpack src <> "\nTokens: " <> show tokens) False

tests :: TestTree
tests = testGroup "Parser tests" [testProperty "roundTrip" prop_roundTrip]