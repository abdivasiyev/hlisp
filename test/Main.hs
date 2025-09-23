module Main (main) where

import Lexer qualified as L
import Parser qualified as P
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "HLisp Tests"
            [ L.tests
            , P.tests
            ]
