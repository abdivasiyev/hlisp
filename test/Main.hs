module Main (main) where

import Lexer (tests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "HLisp Tests"
            [ tests
            ]
