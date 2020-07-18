module Unification where

import Gabs.Ast

import Data.Maybe

import Data.Tuple.Extra
import qualified Data.Map.Strict as Map

-- Right now, this is specific to types. Can it be generalized?

-- Visual example of type inference/unification
-- https://www.cs.colorado.edu/~bec/courses/csci5535-s09/slides/ML_Type_Inference_and_Unification.pdf

data UnifType =
    UTBool
  | UTInt
  | UTArr UnifType UnifType
  | UTHook Integer
  deriving (Eq, Show)

type UnifContext = Map.Map Name UnifType
emptyCont :: UnifContext
emptyCont = Map.empty

type Constraint = (UnifType, UnifType)

type ConstrSolution = Map.Map Integer UnifType
emptySol :: ConstrSolution
emptySol = Map.empty

-- Defines a partial ordering
-- If u `appearsIn` v, then a constraint u = v should be rejected, as it implies
-- an infinite type.
appearsIn :: UnifType -> UnifType -> Bool
appearsIn s = go
  where
    go (UTArr x y) = s `appearsIn` x || s `appearsIn` y
    go x = s == x

-- TODO: make the definitive lookup for ConstrSolutions
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

-- Produces a mapping from unification variables to their principal types
solve :: Foldable f => f Constraint -> Maybe ConstrSolution
solve = solveFold $ Just emptySol
  where
    solveFold :: Foldable f => Maybe ConstrSolution -> f Constraint -> Maybe ConstrSolution
    solveFold = foldr $ (=<<) . go
    go :: Constraint -> ConstrSolution -> Maybe ConstrSolution
    go (l, r) sol =
      let lExp = expandUT sol l in
      let rExp = expandUT sol r in
      if lExp == rExp then
        Just sol
      else if lExp `appearsIn` rExp || rExp `appearsIn` lExp then
        Nothing
      else
        case (lExp, rExp) of
          (UTHook i, _) -> Just $ addToSol i rExp sol
          (_, UTHook i) -> Just $ addToSol i lExp sol
          (UTArr a b, UTArr x y) -> solveFold (Just sol) [(a,x), (b,y)]
          _ -> Nothing

solToConstraints :: ConstrSolution -> [Constraint]
solToConstraints sol = mapOverFst UTHook <$> Map.toList sol
  where
    mapOverFst f (x, y) = (f x, y)

solveWith :: [ConstrSolution] -> [Constraint] -> Maybe ConstrSolution
solveWith sols constrs = solve $ concat $ constrs : (solToConstraints <$> sols)


inferType :: Expr -> Maybe UnifType
inferType = fmap fst3 . go 0 emptyCont
  where
    go :: Integer -> UnifContext -> Expr -> Maybe (UnifType, ConstrSolution, Integer)
    go i cont exp = case exp of
      Norm (B _) -> pure (UTBool, emptySol, i)
      Norm (I _) -> pure (UTInt,  emptySol, i)
      Norm (Lambda _ n _ body) -> do
        let extConv = Map.insert n (UTHook i) cont
        (range, sol, j) <- go (i+1) extConv body
        let domain = expandUT sol (UTHook i)
        pure (UTArr domain range, sol, j)
      Var n -> do
        uType <- Map.lookup n cont
        pure (uType, emptySol, i)
      Fix e -> do
        (UTArr t1 t2, sol, j) <- go i cont e
        if t1 == t2 then
          pure (t1, sol, j)
        else
          Nothing
      Eq e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, t2)]
        pure (UTBool, sol, k)
      Lt e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTBool, sol, k)
      Gt e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTBool, sol, k)
      Lte e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTBool, sol, k)
      Gte e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTBool, sol, k)
      And e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTBool), (t2, UTBool)]
        pure (UTBool, sol, k)
      Or e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTBool), (t2, UTBool)]
        pure (UTBool, sol, k)
      Not e -> do
        (t, sol1, j) <- go i cont e
        sol <- solveWith [sol1] [(t, UTBool)]
        pure (UTBool, sol, j)
      Plus e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTInt, sol, k)
      Minus e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTInt, sol, k)
      Times e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTInt, sol, k)
      Div e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        sol <- solveWith [sol1, sol2] [(t1, UTInt), (t2, UTInt)]
        pure (UTInt, sol, k)
      Ite e1 e2 e3 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        (t3, sol3, l) <- go k cont e3
        sol <- solveWith [sol1, sol2, sol3] [(t1, UTBool), (t2, t3)]
        pure (expandUT sol t2, sol, l)
      App e1 e2 -> do
        (t1, sol1, j) <- go i cont e1
        (t2, sol2, k) <- go j cont e2
        let domain = UTHook $ k + 1
        let range  = UTHook $ k + 2
        sol <- solveWith [sol1, sol2] [(t1, UTArr domain range), (t2, domain)]
        pure (expandUT sol range, sol, k + 2)
