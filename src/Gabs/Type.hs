{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

module Gabs.Type where 

import Gabs.Ast
import Unification

import Data.Functor.Classes
import qualified Data.Map.Strict as Map


data TypeF r
  = TBool
  | TInt
  | TArr r r
  deriving (Eq, Functor, Foldable, Traversable)

instance Eq1 TypeF where 
  liftEq _ TBool TBool = True
  liftEq _ TInt  TInt  = True
  liftEq fEq (TArr a1 b1) (TArr a2 b2) = a1 `fEq` a2 && b1 `fEq` b2
  liftEq _ _ _ =  False

instance Show1 TypeF where
  liftShowsPrec _ _ _ TBool = showString "Bool"
  liftShowsPrec _ _ _ TInt  = showString "Int"
  liftShowsPrec fPrec _ i (TArr r1 r2) = showParen (i > 10)
    $ fPrec 11 r1
    . showString " -> "
    . fPrec 11 r2

type Type = UnifPattern TypeF

type Context = Map.Map Name Type
emptyCont :: Context
emptyCont = Map.empty

inferType :: Expr -> Maybe Type 
inferType = evalUnif . go emptyCont
  where 
    go :: Context -> Expr -> Unifier TypeF Type
    go cont expr = case expr of
      Norm (B _) -> pure $ UBase TBool
      Norm (I _) -> pure $ UBase TInt
      Norm (Lambda _ n body) -> do
        var <- freshVar
        let extCont = Map.insert n (UVar var) cont
        range  <- go extCont body
        domain <- solve $ UVar var
        pure $ UBase $ TArr domain range
      Var n -> 
        liftMaybeToUnif $ Map.lookup n cont 
      Fix e -> do 
        UBase (TArr domain range) <- go cont e
        domain === range
        solve domain
      Eq e1 e2 -> do 
        t1 <- go cont e1 
        t2 <- go cont e2 
        t1 === t2 
        pure $ UBase TBool
      Lt e1 e2 -> do
        t1 <- go cont e1 
        t1 === UBase TInt
        t2 <- go cont e2 
        t2 === UBase TInt
        pure $ UBase TBool
      Gt e1 e2 -> do
        t1 <- go cont e1 
        t1 === UBase TInt
        t2 <- go cont e2 
        t2 === UBase TInt
        pure $ UBase TBool
      Lte e1 e2 -> do
        t1 <- go cont e1 
        t1 === UBase TInt
        t2 <- go cont e2 
        t2 === UBase TInt
        pure $ UBase TBool
      Gte e1 e2 -> do
        t1 <- go cont e1 
        t1 === UBase TInt
        t2 <- go cont e2 
        t2 === UBase TInt
        pure $ UBase TBool
      And e1 e2 -> do
        t1 <- go cont e1 
        t1 === UBase TBool
        t2 <- go cont e2 
        t2 === UBase TBool
        pure $ UBase TBool
      Or e1 e2 -> do
        t1 <- go cont e1 
        t1 === UBase TBool
        t2 <- go cont e2 
        t2 === UBase TBool
        pure $ UBase TBool
      Not e -> do
        t <- go cont e 
        t === UBase TBool
        pure $ UBase TBool
      Plus e1 e2 -> do
        t1 <- go cont e1
        t1 === UBase TInt
        t2 <- go cont e2
        t2 === UBase TInt
        pure $ UBase TInt
      Minus e1 e2 -> do
        t1 <- go cont e1
        t1 === UBase TInt
        t2 <- go cont e2
        t2 === UBase TInt
        pure $ UBase TInt
      Times e1 e2 -> do
        t1 <- go cont e1
        t1 === UBase TInt
        t2 <- go cont e2
        t2 === UBase TInt
        pure $ UBase TInt
      Div e1 e2 -> do
        t1 <- go cont e1
        t1 === UBase TInt
        t2 <- go cont e2
        t2 === UBase TInt
        pure $ UBase TInt
      Ite c t e -> do 
        tC <- go cont c 
        tC === UBase TBool
        tT <- go cont t 
        tE <- go cont e 
        tT === tE
        solve tT
      App e1 e2 -> do 
        t1 <- go cont e1 
        domain <- UVar <$> freshVar 
        range  <- UVar <$> freshVar 
        t1 === UBase (TArr domain range)
        t2 <- go cont e2 
        t2 === domain 
        solve range





