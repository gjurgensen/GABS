module GabsEval where

import GabsAst

type Env = [(Name, Exp)]

evalExp :: Env -> Exp -> Maybe Exp
evalExp env exp = case exp of
  B b -> pure $ B b
  I i -> pure $ I i
  Lambda n t e -> pure $ Lambda n t e
  Var n -> lookup n env
  And e1 e2 -> do
    B b1 <- evalExp env e1
    B b2 <- evalExp env e2
    pure $ B $ b1 && b2
  Or e1 e2 -> do
    B b1 <- evalExp env e1
    B b2 <- evalExp env e2
    pure $ B $ b1 || b2
  Not e -> do
    B b <- evalExp env e
    pure $ B $ not b
  Plus e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure $ I $ i1 + i2
  Minus e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure $ I $ i1 - i2
  Times e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure $ I $ i1 * i2
  Div e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure $ I $ i1 `div` i2
  Ite e1 e2 e3 -> do
    B b <- evalExp env e1
    evalExp env $ if b then e2 else e3
  App e1 e2 -> do
    Lambda n _ body <- evalExp env e1
    e2' <- evalExp env e2 -- CBV
    evalExp ((n, e2') : env) body
