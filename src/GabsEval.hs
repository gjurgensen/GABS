module GabsEval where

import GabsAst

import Control.Monad
import qualified Data.Map.Strict as Map

-- TODO: Syntactically differentiate normal terms, restrict map to normal terms.
-- (Only fully evaluates terms are added to the environment, so this *should*
--  happen anyway.)
type Env = Map.Map Name Exp
emptyEnv = Map.empty :: Env

-- TODO: Only lambdas need their own env, should move env into ast, out of eval
eval :: Env -> Exp -> Maybe (Env, Exp)
eval env exp = case exp of
  B b -> pure (env, B b)
  I i -> pure (env, I i)
  Lambda n t e -> pure (env, Lambda n t e)
  Var n -> (,) env <$> Map.lookup n env
  And e1 e2 -> do
    B b1 <- evalExp env e1
    B b2 <- evalExp env e2
    pure (env, B $ b1 && b2)
  Or e1 e2 -> do
    B b1 <- evalExp env e1
    B b2 <- evalExp env e2
    pure (env, B $ b1 || b2)
  Not e -> do
    B b <- evalExp env e
    pure (env, B $ not b)
  Plus e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure (env, I $ i1 + i2)
  Minus e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure (env, I $ i1 - i2)
  Times e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure (env, I $ i1 * i2)
  Div e1 e2 -> do
    I i1 <- evalExp env e1
    I i2 <- evalExp env e2
    pure (env, I $ i1 `div` i2)
  Ite e1 e2 e3 -> do
    B b <- evalExp env e1
    eval env $ if b then e2 else e3
  App e1 e2 -> do
      (envLam, Lambda n _ body) <- eval env e1
      e2' <- evalExp env e2 -- CBV
      let extEnv = Map.insert n e2' envLam
      eval extEnv body
  where
    evalExp env expr = snd <$> eval env expr
    printMaybeResult (Just r) = printResult r
    printMaybeResult Nothing  = putStrLn "Nothing"

printResult :: (Env, Exp) -> IO ()
printResult (env, exp) = do
  putStrLn $ show exp
  when (isLambda exp) $
    putStrLn $ "Environment: " ++ show env
  where
    isLambda (Lambda _ _ _) = True
    isLambda _ = False
