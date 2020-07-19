module Gabs.Type where

import Gabs.Ast

import Data.Char
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
  deriving Eq

instance Show UnifType where
  show UTBool = "Bool"
  show UTInt  = "Int"
  show (UTHook i) = reverse $ intToName i
  show (UTArr t1@(UTArr _ _) t2) = parens (show t1) ++ " -> " ++ show t2
    where parens s = "(" ++ s ++ ")"
  show (UTArr t1 t2) = show t1 ++ " -> " ++ show t2

intToName = reverse . go
  where
    go i =
      if i < 26 then
        [chr $ fromIntegral $ 97 + i]
      else
        let (d, m) = i `divMod` 26 in
        (chr $ fromIntegral $ 97 + m) : go (d - 1)

normalizeUT = fst3 . norm 0 Map.empty
  where
    norm i m UTBool = (UTBool, m, i)
    norm i m UTInt  = (UTInt,  m, i)
    norm i m (UTArr t1 t2) =
      let (t1', m1, j) = norm i m  t1 in
      let (t2', m2, k) = norm j m1 t2 in
      (UTArr t1' t2', m2, k)
    norm i m (UTHook j) = case Map.lookup j m of
      Just n  -> (UTHook n, m, i)
      Nothing -> (UTHook i, Map.insert j i m, i+1)


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
-- better name: subsumes? occursIn?
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
solToConstraints sol = mapFst UTHook <$> Map.toList sol
  where
    mapFst f (x, y) = (f x, y)

solveWith :: [ConstrSolution] -> [Constraint] -> Maybe ConstrSolution
solveWith sols constrs = solve $ concat $ constrs : (solToConstraints <$> sols)


inferType :: Expr -> Maybe UnifType
inferType = fmap fst3 . go 0 emptyCont
  where
    go :: Integer -> UnifContext -> Expr -> Maybe (UnifType, ConstrSolution, Integer)
    go i cont exp = case exp of
      Norm (B _) -> pure (UTBool, emptySol, i)
      Norm (I _) -> pure (UTInt,  emptySol, i)
      Norm (Lambda _ n body) -> do
        let extConv = Map.insert n (UTHook i) cont
        (range, sol, j) <- go (i+1) extConv body
        let domain = expandUT sol (UTHook i)
        pure (UTArr domain range, sol, j)
      Var n -> do
        uType <- Map.lookup n cont
        pure (uType, emptySol, i)
      Fix e -> do
        (UTArr t1 t2, sol1, j) <- go i cont e
        sol <- solveWith [sol1] [(t1, t2)]
        pure (expandUT sol t1, sol, j)
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
        let domain = UTHook $ k
        let range  = UTHook $ k + 1
        sol <- solveWith [sol1, sol2] [(t1, UTArr domain range), (t2, domain)]
        pure (expandUT sol range, sol, k + 2)
