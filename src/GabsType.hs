module GabsType where

import GabsAst

import qualified Data.Map.Strict as Map

type Context = Map.Map Name Type
emptyContext = Map.empty :: Context

typeExp :: Context -> Exp -> Maybe Type
typeExp cont exp = case exp of
  B _ -> pure TBool
  I _ -> pure TInt
  Lambda n t body -> do
    let extConv = Map.insert n t cont
    tBody <- typeExp extConv body
    pure $ TArr t tBody
  Var n -> Map.lookup n cont
  And e1 e2 -> do
    TBool <- typeExp cont e1
    TBool <- typeExp cont e2
    pure TBool
  Or e1 e2 -> do
    TBool <- typeExp cont e1
    TBool <- typeExp cont e2
    pure TBool
  Not e -> do
    TBool <- typeExp cont e
    pure TBool
  Plus e1 e2 -> do
    TInt <- typeExp cont e1
    TInt <- typeExp cont e2
    pure TInt
  Minus e1 e2 -> do
    TInt <- typeExp cont e1
    TInt <- typeExp cont e2
    pure TInt
  Times e1 e2 -> do
    TInt <- typeExp cont e1
    TInt <- typeExp cont e2
    pure TInt
  Div e1 e2 -> do
    TInt <- typeExp cont e1
    TInt <- typeExp cont e2
    pure TInt
  Ite e1 e2 e3 -> do
    TBool <- typeExp cont e1
    t1 <- typeExp cont e2
    t2 <- typeExp cont e3
    if t1 == t2 then pure t1 else fail ""
  App e1 e2 -> do
    TArr t1 t2 <- typeExp cont e1
    t3 <- typeExp cont e2
    if t1 == t3 then pure t2 else fail ""
