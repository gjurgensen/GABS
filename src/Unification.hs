module Unification where

import GabsAst

import Data.Maybe

import qualified Data.Map.Strict as Map

data UnifType =
    UTBool
  | UTInt
  | UTArr UnifType UnifType
  | UTHook Integer
  deriving (Eq, Show)

type UnifContext = Map.Map Name UnifType
type Constraint  = (UnifType, UnifType)
type Constraints = [Constraint]
emptyConstr = []

-- -- Maps unification variables to their principal types
-- solveConstraints :: Constraints -> Map.Map UnifType UnifType

-- appearsIn :: UnifType -> UnifType -> Bool
-- appearsIn s = go
--   where
--     go (UTArr x y) = s `appearsIn` x || s `appearsIn` y
--     go x = s == x
--
-- type UnifMap = Map.Map Integer UnifType
-- unifyTypes :: Constraints -> UnifMap -> UnifMap
-- unifyTypes [] _ = --empty
-- unifyTypes ((i, t) :: cs) m =
--   let it = fromMaybe (UTHook i) $ Map.lookup i m
--   if it = t then
--     unifyTypes cs
--   else if not $ it `appearsIn` t then
--     unifyTypes (Map.insert i t m)
--   else if not $ t `appearsIn` it then
--     unifyTypes (Map.insert i t m)

-- Start over
-- todo write as a fold
-- Visual example of type inference/unification
-- https://www.cs.colorado.edu/~bec/courses/csci5535-s09/slides/ML_Type_Inference_and_Unification.pdf

type ConstrSolution = Map.Map Integer UnifType
emptySol :: ConstrSolution
emptySol = Map.empty

expandUT :: ConstrSolution -> UnifType -> UnifType
expandUT sol = go
  where
    go (UTArr ut1 ut2) = UTArr (go ut1) (go ut2)
    go x@(UTHook i) = fromMaybe x $ Map.lookup i sol
    go x = x

-- Adds mapping to solution, and also applies that mapping throughout its own
-- range, such that no unification variable in the domain appears in the terms
-- in the range
addToSol :: Integer -> UnifType -> ConstrSolution -> ConstrSolution
addToSol i ut sol = Map.insert i ut $ subst i ut <$> sol
  where
    subst :: Integer -> UnifType -> UnifType -> UnifType
    subst i t = expandUT $ Map.singleton i t

-- TODO: Check for cycles
--   i.e. guard by "contained in".
--   A constraint like `a = b -> a` should be caught as infinite
solveConstraints :: Foldable f => f Constraint -> Maybe ConstrSolution
solveConstraints = constrFold $ Just emptySol
  where
    constrFold :: Foldable f => Maybe ConstrSolution -> f Constraint -> Maybe ConstrSolution
    constrFold = foldr $ (=<<) . go
    go :: Constraint -> ConstrSolution -> Maybe ConstrSolution
    go (l, r) sol =
      let lExp = expandUT sol l in
      let rExp = expandUT sol r in
      case (lExp, rExp) of
        (UTHook i, _) -> Just $ addToSol i rExp sol
        (_, UTHook i) -> Just $ addToSol i lExp sol
        (UTArr a b, UTArr x y) -> constrFold (Just sol) [(a,x), (b,y)]
        (x, y) -> if x == y then Just sol else Nothing


-- annotType :: Integer -> UnifContext -> Expr -> (UnifType, Constraints)
-- annotType i cont exp = case exp of
--   Norm (B _) -> (UTBool, emptyConstr)
--   Norm (I _) -> (UTInt,  emptyConstr)
--   Norm (Lambda _ n _ body) ->
--     let extConv = Map.insert n (UTHook i) cont in
--     let (typeBody, constrBody) = annotType extConv body in
--     (UTArr )
--
--   Norm (Lambda _ n _ body) -> do
--     let extConv = Map.insert n i cont
--     tBody <- typeExp extConv body
--     pure $ TArr t tBody
--   Var n -> Map.lookup n cont
--   Fix e -> do
--     TArr t1 t2 <- typeExp cont e
--     if t1 == t2 then pure t1 else fail ""
--   Eq e1 e2 -> do
--     t1 <- typeExp cont e1
--     t2 <- typeExp cont e1
--     if t1 == t2 then pure TBool else fail ""
--   Lt e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e1
--     pure $ Type TBool
--   Gt e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e1
--     pure $ Type TBool
--   Lte e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e1
--     pure $ Type TBool
--   Gte e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e1
--     pure $ Type TBool
--   And e1 e2 -> do
--     TBool <- typeExp cont e1
--     TBool <- typeExp cont e2
--     pure $ Type TBool
--   Or e1 e2 -> do
--     TBool <- typeExp cont e1
--     TBool <- typeExp cont e2
--     pure $ Type TBool
--   Not e -> do
--     TBool <- typeExp cont e
--     pure $ Type TBool
--   Plus e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e2
--     pure $ Type TInt
--   Minus e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e2
--     pure $ Type TInt
--   Times e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e2
--     pure $ Type TInt
--   Div e1 e2 -> do
--     TInt <- typeExp cont e1
--     TInt <- typeExp cont e2
--     pure $ Type TInt
--   Ite e1 e2 e3 -> do
--     TBool <- typeExp cont e1
--     t1 <- typeExp cont e2
--     t2 <- typeExp cont e3
--     if t1 == t2 then pure t1 else fail ""
--   App e1 e2 -> do
--     TArr t1 t2 <- typeExp cont e1
--     t3 <- typeExp cont e2
--     if t1 == t3 then pure t2 else fail ""
