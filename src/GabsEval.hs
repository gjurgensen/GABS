module GabsEval where

import GabsAst

import Control.Monad
import qualified Data.Map.Strict as Map

eval :: Env -> Expr -> Maybe NormalExpr
eval env expr = case expr of
  Norm (B b) -> pure $ B b
  Norm (I i) -> pure $ I i
  Norm (Lambda _ n t b) -> pure $ Lambda env n t b
  Var  n -> Map.lookup n env
  And e1 e2 -> do
    B b1 <- eval env e1
    B b2 <- eval env e2
    pure $ B $ b1 && b2
  Or e1 e2 -> do
    B b1 <- eval env e1
    B b2 <- eval env e2
    pure $ B $ b1 || b2
  Not e -> do
    B b <- eval env e
    pure $ B $ not b
  Plus e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ I $ i1 + i2
  Minus e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ I $ i1 - i2
  Times e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ I $ i1 * i2
  Div e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ I $ i1 `div` i2
  Ite e1 e2 e3 -> do
    B b <- eval env e1
    eval env $ if b then e2 else e3
  App e1 e2 -> do
    Lambda envLam n _ body <- eval env e1
    e2' <- eval env e2 -- CBV
    let extEnv = Map.insert n e2' envLam
    eval extEnv body

printResult :: NormalExpr -> IO ()
printResult nExpr = do
  putStrLn $ show nExpr
  case nExpr of
    Lambda env _ _ _ -> putStrLn $ "Environment: " ++ show env
    _ -> nop
  where
    nop = pure ()
