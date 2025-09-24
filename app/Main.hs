{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text (Text)
import Data.Text qualified as T
import HLisp.Eval
import HLisp.Parser
import HLisp.Repl (repl)
import Text.Pretty.Simple (pPrint)

prog :: Text
prog =
  T.unlines
    [ "(defvar searchspace '(1 5 10 15 20 25))"
    , "(defvar target 20)"
    , "(defvar lo 0)"
    , "(defvar high (- (list-length searchspace) 1))"
    , "(defvar mid)"
    , "(defun bin_search (listToSearch)"
    , "    (setq mid (round (/ (+ lo high) 2)))"
    , "    (if (eq (nth mid listToSearch) target)"
    , "        (return-from bin_search mid)"
    , "        (if (> (nth mid listToSearch) target)"
    , "            (setq high (- mid 1))"
    , "            (setq lo (+ mid 1))"
    , "        )"
    , "    )"
    , "    (bin_search listToSearch)"
    , ")"
    , ""
    , "(print (bin_search searchspace))"
    ]

-- main :: IO ()
-- main = case parseSrc prog of
--   Left err -> putStrLn $ "Parse error: " ++ err
--   Right expr -> do
--     (result, env) <- runEval baseEnv (eval expr)
--     case result of
--       Left evalErr -> putStrLn $ "Evaluation error: " ++ evalErr
--       Right val -> do
--         pPrint val
--         pPrint env

main :: IO ()
main = repl
