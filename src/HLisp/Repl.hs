{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HLisp.Repl (
    repl,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import HLisp.Eval (Env, Value, baseEnv, eval, pretty, runEval)
import HLisp.Parser (parseSrc)
import System.Console.Haskeline

repl :: IO ()
repl = do
    putStrLn "Welcome to HLisp REPL! Type :quit to exit or Ctrl+D."
    runInputT defaultSettings $ loop baseEnv
  where
    loop env = do
        minput <- getInputLine "HLisp> "
        case minput of
            Nothing -> outputStrLn "Goodbye."
            Just ":quit" -> outputStrLn "Goodbye."
            Just input -> do
                (result, env') <- liftIO $ interpretLine env (T.pack input)
                case result of
                    Left err -> outputStrLn $ "Error: " ++ err
                    Right val -> outputStrLn $ pretty val
                loop env'

interpretLine :: Env -> T.Text -> IO (Either String Value, Env)
interpretLine env line = do
    case parseSrc line of
        Left parseErr -> pure (Left parseErr, env)
        Right expr -> runEval env (eval expr)
