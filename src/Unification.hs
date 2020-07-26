-- Inspired by the unification-fd library

{-# LANGUAGE KindSignatures #-}

module Unification (
  UnifPattern(..),
  UnifierT, Unifier,
  occursIn,
  getPrincipal,
  addConstraint, (===),
  freshVar, 
  solve,
  liftMaybeToUnif,
  runUnifT, runUnif,
  evalUnifT, evalUnif,
  execUnifT, execUnif
  ) where

import Data.Char
import Data.Maybe
import Data.Foldable
import Control.Monad
import Data.Functor.Identity

import Data.Tuple.Extra
import Data.Functor.Classes
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as Map


-- Conceptually: a wrapper around a state monad, where constraints/solutions
-- accumulate over binds, and fresh variable names are managed behind the scenes

type Var = Int

data UnifPattern (t :: * -> *)
  = UBase (t (UnifPattern t))
  | UVar Var

instance Eq1 t => Eq (UnifPattern t) where
  UVar  v1 == UVar  v2 = v1 == v2
  UBase t1 == UBase t2 = liftEq (==) t1 t2
  _ == _ = False

instance Show1 t => Show (UnifPattern t) where
  show (UVar  v) = varToName v
  show (UBase t) = showsPrec1 0 t ""

varToName :: Var -> String
varToName = reverse . go
  where
    go i =
      if i < 26 then
        [chr $ fromIntegral $ 97 + i]
      else
        let (d, m) = i `divMod` 26 in
        (chr $ fromIntegral $ 97 + m) : go (d - 1)

type Constraint t = (UnifPattern t, UnifPattern t)
type Solution t = Map.Map Var (UnifPattern t) -- TODO replace with intmap?


data UnifyState t = UnifyState
  { nextVar  :: Var
  , solution :: Solution t
  }

emptyState = UnifyState
  { nextVar  = 0
  , solution = Map.empty
  }

data UnifierT t m a = UnifierT (StateT (UnifyState t) (MaybeT m) a)
type Unifier t = UnifierT t Identity

unwrapUnifierT (UnifierT stateM) = stateM

emptyUnifier :: Monad m => UnifierT t m ()
emptyUnifier = UnifierT $ put emptyState

addToSol :: Functor t => Var -> UnifPattern t -> Solution t -> Solution t
addToSol v u sol = Map.insert v u $ subst <$> sol 
  where
    subst = getPrincipal $ Map.singleton v u

-- TODO: make `Unifiable` type class, with the following `unify` and `occursIn`
--   definitions for instance (Eq1 t, Functor t, Foldable t) => Unifiable t
unify :: (Eq1 t, Functor t, Foldable t)
  => Constraint t
  -> Solution t
  -> Maybe (Solution t)
unify constr sol = withPrincipals (both (getPrincipal sol) constr) sol 
  where 
    u1 `related` u2 = u1 `occursIn` u2 || u2 `occursIn` u1
    sameStruct = liftEq $ const $ const True
    withPrincipals (u1, u2) sol =
      if u1 == u2 then
        Just sol
      else if u1 `related` u2 then 
        Nothing
      else 
        case (u1, u2) of 
          (UVar v, _) -> Just $ addToSol v u2 sol 
          (_, UVar v) -> Just $ addToSol v u1 sol
          (UBase b1, UBase b2) -> 
            if b1 `sameStruct` b2 then
              foldrM unify sol $ zip (toList b1) (toList b2)
            else
              Nothing

unifySolutions :: (Eq1 t, Functor t, Foldable t)
  => Solution t
  -> Solution t
  -> Maybe (Solution t)
--unifySolutions sol = foldr ((=<<) . unify) (Just sol) . solToConstrs
unifySolutions sol = foldrM unify sol . solToConstrs
  where
    solToConstrs sol = mapFst UVar <$> Map.toList sol
    mapFst f (x, y) = (f x, y)

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure


-- Exported definitions:

occursIn :: (Eq1 t, Foldable t) => UnifPattern t -> UnifPattern t -> Bool
x `occursIn` y = x == y || case (x,y) of
  (x, UBase y) -> any (x `occursIn`) y
  _ -> False

getPrincipal :: Functor t => Solution t -> UnifPattern t -> UnifPattern t
getPrincipal sol = go 
  where 
    go v@(UVar var) = fromMaybe v $ Map.lookup var sol
    go (UBase base) = UBase $ go <$> base

addConstraint :: (Eq1 t, Functor t, Foldable t, Monad m)
  => Constraint t
  -> UnifierT t m ()
addConstraint c = UnifierT $ do
  state <- get
  void $ lift $ do
    newSol <- liftMaybe $ unify c $ solution state
    pure $ state {solution = newSol}

(===) :: (Eq1 t, Functor t, Foldable t, Monad m)
  => UnifPattern t
  -> UnifPattern t
  -> UnifierT t m ()
(===) = curry addConstraint

freshVar :: Monad m => UnifierT t m Var
freshVar = UnifierT $ do
  var <- gets nextVar
  modify $ \s -> s {nextVar = var + 1}
  pure var

solve :: Functor t => UnifPattern t -> Unifier t (UnifPattern t)
solve t = UnifierT $ do 
  sol <- gets solution
  pure $ getPrincipal sol t

liftMaybeToUnif :: Monad m => Maybe a -> UnifierT t m a
liftMaybeToUnif = UnifierT . lift . liftMaybe

runUnifT :: Monad m => UnifierT t m a -> MaybeT m (a, Solution t)
runUnifT (UnifierT stateM) = mapSnd solution <$> runStateT stateM emptyState
  where
    mapSnd f (x, y) = (x, f y)

runUnif :: Unifier t a -> Maybe (a, Solution t)
runUnif = runIdentity . runMaybeT . runUnifT 

evalUnifT :: Monad m => UnifierT t m a -> MaybeT m a
evalUnifT = fmap fst . runUnifT

evalUnif :: Unifier t a -> Maybe a 
evalUnif = runIdentity . runMaybeT . evalUnifT

execUnifT :: Monad m => UnifierT t m a -> MaybeT m (Solution t)
execUnifT = fmap snd . runUnifT

execUnif :: Unifier t a -> Maybe (Solution t)
execUnif = runIdentity . runMaybeT . execUnifT

instance (Eq1 t, Monad m) => Functor (UnifierT t m) where
  fmap f (UnifierT ma) = UnifierT $ fmap f ma

instance (Eq1 t, Functor t, Foldable t, Monad m) => Applicative (UnifierT t m) where
  pure = UnifierT . pure
  UnifierT mf <*> UnifierT mx = UnifierT $ do
    f <- mf
    UnifyState {nextVar = var1, solution = sol1} <- get
    x <- mx
    UnifyState {nextVar = var2, solution = sol2} <- get
    lift $ do
      newSol <- liftMaybe $ unifySolutions sol1 sol2
      pure $ UnifyState {nextVar = max var1 var2, solution = newSol}
    pure $ f x

instance (Eq1 t, Functor t, Foldable t, Monad m) => Monad (UnifierT t m) where
  UnifierT ma >>= f = UnifierT $ do
    a <- ma
    UnifyState {nextVar = var1, solution = sol1} <- get
    b <- unwrapUnifierT $ f a
    UnifyState {nextVar = var2, solution = sol2} <- get
    lift $ do
      newSol <- liftMaybe $ unifySolutions sol1 sol2
      pure $ UnifyState {nextVar = max var1 var2, solution = newSol}
    pure b

instance (Eq1 t) => MonadTrans (UnifierT t) where
  lift = UnifierT . lift . lift

instance (Eq1 t, Functor t, Foldable t, Monad m) => MonadFail (UnifierT t m) where
  fail msg = UnifierT $ lift $ fail msg