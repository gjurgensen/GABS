module GabsEval where

import GabsAst

import Control.Monad
import qualified Data.Map.Strict as Map

-- How to handle equality of lambdas:
-- 1. Structural equality after substitutions, and with eta reduction
--    - eta equality complexity = unification complexity
-- 2. Always return false (or true, or error). The logic here is that users
--    might expect extensional equality, which is undecidable, and it may be
--    best not even try

eval :: Env -> Expr -> Maybe NormalExpr
eval env expr = case expr of
  Norm (B b) -> pure $ B b
  Norm (I i) -> pure $ I i
  Norm (Lambda _ n t b) -> pure $ Lambda env n t b
  Var n -> Map.lookup n env >>= eval env
  Fix e -> do
    lam@(Lambda envLam n t body) <- eval env e
    eval (Map.insert n (Fix $ Norm lam) envLam) body
  Eq e1 e2 -> do
    e1' <- eval env e1
    e2' <- eval env e2
    pure $ B $ e1' == e2' -- lambda equality is ill-defined, see above
  Lt e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ B $ i1 < i2
  Gt e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ B $ i1 > i2
  Lte e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ B $ i1 <= i2
  Gte e1 e2 -> do
    I i1 <- eval env e1
    I i2 <- eval env e2
    pure $ B $ i1 >= i2
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
    let extEnv = Map.insert n (Norm e2') envLam
    eval extEnv body

-- Prints a lambda's environment.
printResult :: NormalExpr -> IO ()
printResult nExpr = do
  putStrLn $ show nExpr
  case nExpr of
    Lambda env _ _ _ -> putStrLn $ "Environment: " ++ show env
    _ -> nop
  where
    nop = pure ()
