{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module HLisp.Eval (
    eval,
    runEval,
    Value (..),
    Env,
    baseEnv,
    pretty,
) where

import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Text qualified as T

import Control.Monad.IO.Class (MonadIO (liftIO))
import HLisp.Parser (Expr (..))

type Env = M.Map Text Value
type EvalT a = ExceptT String (StateT Env IO) a

baseEnv :: Env
baseEnv = M.fromList primitives

newtype Primitive = Primitive {runPrimitive :: [Value] -> EvalT Value}

type SpecialForm = [Expr] -> EvalT Value

instance Show Primitive where
    show _ = "<primitive>"

instance Eq Primitive where
    _ == _ = False

instance Ord Primitive where
    compare _ _ = EQ

data Value
    = VSymbol Text
    | VNumber Integer
    | VString Text
    | VBool Bool
    | VDotted Value Value
    | VUnquote Value
    | VUnquoteSplicing Value
    | VList [Value]
    | VClosure [Text] [Expr] Env
    | VPrimitive Primitive
    | VNil
    deriving (Show, Eq, Ord)

pretty :: Value -> String
pretty (VSymbol s) = T.unpack s
pretty (VNumber n) = show n
pretty (VString s) = show s
pretty (VBool True) = "#t"
pretty (VBool False) = "#f"
pretty dotted@(VDotted _ _) =
    "(" <> go dotted <> ")"
  where
    go (VDotted a b) = case b of
        VList xs -> unwords (pretty a : map pretty xs)
        VDotted _ _ ->
            let left = pretty a
                right = go b
             in left <> " " <> right
        _ -> pretty a <> " . " <> pretty b
    go v = pretty v
pretty (VUnquote v) = "," ++ pretty v
pretty (VUnquoteSplicing v) = ",@" ++ pretty v
pretty (VList vs) = "(" ++ unwords (map pretty vs) ++ ")"
pretty VClosure{} = "<closure>"
pretty (VPrimitive _) = "<primitive>"
pretty VNil = "nil"

-- | Evaluate an expression in a given environment
eval :: Expr -> EvalT Value
eval = \case
    ENumber n -> pure $ VNumber n
    EBool b -> return $ VBool b
    EString s -> return $ VString s
    ESymbol s -> lookupVar s
    EDotted car cdr -> do
        car' <- mapM eval car
        cdr' <- eval cdr
        return $ foldr VDotted cdr' car'
    EList [] -> return $ VList [] -- empty list is a self-evaluating expression
    EList [e] -> eval e -- single element list evaluates to that element, it is because of how parser works
    -- EList [ESymbol "quote", expr] -> pure (evalQuote expr)
    -- EList [ESymbol "defvar", ESymbol name, expr] -> do
    --     val <- eval expr
    --     env <- lift get
    --     lift $ put (M.insert name val env)

    --     pure val
    EList (ESymbol symbol : args) -> do
        case M.lookup symbol specialForms of
            Just form -> form args
            otherwise -> do
                fnVal <- eval (ESymbol symbol)
                argVals <- mapM eval args
                apply fnVal argVals
    EList (fn : args) -> do
        fnVal <- eval fn
        argVals <- mapM eval args
        apply fnVal argVals
    unknown -> throwError $ "Unknown expression: " ++ show unknown

apply :: Value -> [Value] -> EvalT Value
apply (VPrimitive (Primitive f)) args = f args
apply (VClosure params body env) args = applyClosure params body env args
apply notFn _ = throwError $ "Attempted to call a non-function: " ++ show notFn

applyClosure :: [Text] -> [Expr] -> Env -> [Value] -> EvalT Value
applyClosure params body closureEnv args = do
    when (length params /= length args) $
        throwError "Arity mismatch in function application"

    -- store current environment
    env <- lift get
    -- extend environment with parameters bound to arguments
    let env' = M.fromList (zip params args) <> closureEnv
    -- evaluate body in the new environment
    lift $ put env' -- restore previous environment after evaluation
    result <- evalBody body
    lift $ put env -- restore previous environment after evaluation
    return result
  where
    evalBody [e] = eval e
    evalBody (e : es) = eval e >> evalBody es
    evalBody [] = return $ VList []

lookupVar :: Text -> EvalT Value
lookupVar var = do
    env <- lift get

    case M.lookup var env of
        Just val -> return val
        Nothing -> throwError $ "Unbound variable: " ++ show var

primitives :: [(Text, Value)]
primitives =
    [
        ( "+"
        , VPrimitive . Primitive $ \case
            [VNumber x, VNumber y] -> return $ VNumber (x + y)
            args -> throwError $ "Invalid arguments to +: " ++ show args
        )
    ,
        ( "-"
        , VPrimitive . Primitive $ \case
            [VNumber x, VNumber y] -> return $ VNumber (x - y)
            args -> throwError $ "Invalid arguments to -: " ++ show args
        )
    ,
        ( "*"
        , VPrimitive . Primitive $ \case
            [VNumber x, VNumber y] -> return $ VNumber (x * y)
            args -> throwError $ "Invalid arguments to *: " ++ show args
        )
    ,
        ( "/"
        , VPrimitive . Primitive $ \case
            [VNumber x, VNumber y] ->
                if y == 0
                    then fail "Division by zero"
                    else return $ VNumber (x `div` y)
            args -> throwError $ "Invalid arguments to /: " ++ show args
        )
    ,
        ( "="
        , VPrimitive . Primitive $ \case
            [x, y] -> return $ VBool (x == y)
            args -> throwError $ "Invalid arguments to =: " ++ show args
        )
    ,
        ( "eq"
        , VPrimitive . Primitive $ \case
            [x, y] -> return $ VBool (x == y)
            args -> throwError $ "Invalid arguments to =: " ++ show args
        )
    ,
        ( ">"
        , VPrimitive . Primitive $ \case
            [x, y] -> return $ VBool (x > y)
            args -> throwError $ "Invalid arguments to >: " ++ show args
        )
    ,
        ( "<"
        , VPrimitive . Primitive $ \case
            [x, y] -> return $ VBool (x < y)
            args -> throwError $ "Invalid arguments to <: " ++ show args
        )
    ,
        ( "list"
        , VPrimitive . Primitive $ \args -> return $ VList args
        )
    ,
        ( "length"
        , VPrimitive . Primitive $ \case
            [VList xs] -> return $ VNumber (fromIntegral $ length xs)
            [bad] -> throwError $ "length expects a list, got: " ++ show bad
            args -> throwError $ "length arity error, got: " ++ show args
        )
    ,
        ( "car"
        , VPrimitive . Primitive $ \case
            [VList (x : _)] -> return x
            [VList []] -> fail "car called on empty list"
            [bad] -> throwError $ "car expects a list, got: " ++ show bad
            args -> throwError $ "car arity error, got: " ++ show args
        )
    ,
        ( "cdr"
        , VPrimitive . Primitive $ \case
            [VList (_ : xs)] -> return $ VList xs
            [VList []] -> fail "cdr called on empty list"
            [bad] -> throwError $ "cdr expects a list, got: " ++ show bad
            args -> throwError $ "cdr arity error, got: " ++ show args
        )
    ,
        ( "cons"
        , VPrimitive . Primitive $ \case
            [x, VList xs] -> return $ VList (x : xs)
            [_, bad] -> throwError $ "cons expects a list as second argument, got: " ++ show bad
            args -> throwError $ "cons arity error, got: " ++ show args
        )
    ,
        ( "list-length"
        , VPrimitive . Primitive $ \case
            [VList xs] -> return $ VNumber (fromIntegral $ length xs)
            [bad] -> throwError $ "list-length expects a list, got: " ++ show bad
            args -> throwError $ "list-length arity error, got: " ++ show args
        )
    ,
        ( "print"
        , VPrimitive . Primitive $ \args -> case args of
            [v] -> liftIO (print v) >> pure VNil
            _ -> throwError $ "print expects a single argument, got: " ++ show args
        )
    ]

evalQuote :: Expr -> Value
evalQuote (ESymbol s) = VSymbol s
evalQuote (ENumber n) = VNumber n
evalQuote (EBool b) = VBool b
evalQuote (EString s) = VString s
evalQuote (EList xs) = VList (map evalQuote xs)
evalQuote (EDotted car cdr) =
    let head' = map evalQuote car
        tail' = evalQuote cdr
     in foldr VDotted tail' head'
evalQuote unknown = error $ "Cannot quote unknown expression: " ++ show unknown

sfQuote :: SpecialForm
sfQuote [expr] = pure (evalQuote expr)
sfQuote _ = throwError $ "quote expects only 1 argument"

sfDefvar :: SpecialForm
sfDefvar [ESymbol name, expr] = do
    val <- eval expr
    env <- lift get
    lift $ put (M.insert name val env)

    pure val
sfDefvar bad = throwError $ "defvar expects a variable name and expression" <> show bad

specialForms :: M.Map Text SpecialForm
specialForms = M.fromList
    [ ("defvar", sfDefvar)
    , ("quote", sfQuote)
    ]

-- | Run the evaluator with an initial environment and return the result
runEval :: Env -> EvalT a -> IO (Either String a, Env)
runEval env e = runStateT (runExceptT e) env
