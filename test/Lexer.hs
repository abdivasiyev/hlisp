{-# LANGUAGE OverloadedStrings #-}

module Lexer (tests) where

import Data.Text qualified as T
import HLisp.Lexer (tokenize)
import HLisp.Token (Token (..))

import Token (ppToken)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck

tests :: TestTree
tests =
    testGroup
        "Lexer Tests"
        [ testProperty "roundTrip" prop_roundTrip
        ]

prop_roundTrip :: [Token] -> Property
prop_roundTrip tokens =
    let src = T.unwords . map ppToken $ tokens
     in case tokenize src of
            Left err -> counterexample ("Lexer error: " ++ show err ++ "\nSrc: " ++ T.unpack src) False
            Right tokens' -> counterexample ("Expected: " ++ show tokens ++ "\nGot: " ++ show tokens') (tokens' === tokens)