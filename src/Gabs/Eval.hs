module Gabs.Eval where

import Gabs.Ast

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
  Norm (Lambda _ n b) -> pure $ Lambda env n b
  Var n -> Map.lookup n env >>= eval env
  Fix e -> do
    lam@(Lambda envLam n body) <- eval env e
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
    Lambda envLam n body <- eval env e1
    e2' <- eval env e2 -- CBV
    let extEnv = Map.insert n (Norm e2') envLam
    eval extEnv body

desugarNormal :: SugarNormalExpr -> NormalExpr
desugarNormal s = case s of
  SB b -> B b
  SI i -> I i
  SLambda env name s -> Lambda env name $ desugarExpr s

desugarExpr :: SugarExpr -> Expr
desugarExpr s = case s of
  SNorm s -> Norm $ desugarNormal s
  SVar v -> Var v
  SFix s -> Fix $ desugarExpr s
  SIte s1 s2 s3 -> Ite (desugarExpr s1) (desugarExpr s2) (desugarExpr s3)
  SEq s1 s2 -> Eq (desugarExpr s1) (desugarExpr s2)
  SLt s1 s2 -> Lt (desugarExpr s1) (desugarExpr s2)
  SGt s1 s2 -> Gt (desugarExpr s1) (desugarExpr s2)
  SLte s1 s2 -> Lte (desugarExpr s1) (desugarExpr s2)
  SGte s1 s2 -> Gte (desugarExpr s1) (desugarExpr s2)
  SApp s1 s2 -> App (desugarExpr s1) (desugarExpr s2)
  SAnd s1 s2 -> And (desugarExpr s1) (desugarExpr s2)
  SOr s1 s2 -> Or (desugarExpr s1) (desugarExpr s2)
  SNot s -> Not $ desugarExpr s
  SPlus s1 s2 -> Plus (desugarExpr s1) (desugarExpr s2)
  SMinus s1 s2 -> Minus (desugarExpr s1) (desugarExpr s2)
  STimes s1 s2 -> Times (desugarExpr s1) (desugarExpr s2)
  SDiv s1 s2 -> Div (desugarExpr s1) (desugarExpr s2)
  LetIn n s1 s2 -> App (Norm $ Lambda emptyEnv n $ desugarExpr s2) $ desugarExpr s1
  LetRec n s1 s2 -> desugarExpr $ LetIn n (SFix $ SNorm $ SLambda emptyEnv n s1) s2

evalSugar env = eval env . desugarExpr
