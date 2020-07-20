module Unification where

-- state monad for keeping track if id?
-- work over arbitrary types?
--  - I think it'll need some function definitions, such as occursIn, etc.

import Control.Monad
import Data.Functor.Identity

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import qualified Data.Map.Strict as Map

-- Functions needed: unify, varToT

-- Conceptually: a wrapper around a state monad, where constraints are accumulated
-- over binds, and state is guarded.

type Var = Int
type Constraint t = (t, t)
type Solution t = Map.Map Var t --Todo replace with intmap?

-- This version accumulates constraints to be solved at the end
-- data UnifyState t = UnifyState
--   { nextId      :: Var
--   , constraints :: [(t, t)]
--   , solution    :: Map.Map Var t -- Todo replace with intmap?
--   }
--
-- emptyState = UnifyState
--   { nextId      = 0
--   , constraints = []
--   , solution    = Map.empty
--   }

-- This version solves as it goes
data UnifyState t = UnifyState
  { nextId   :: Var
  , solution :: Solution t
  }

emptyState = UnifyState
  { nextId   = 0
  , solution = Map.empty
  }

data UnifierT t m a = UnifierT (StateT (UnifyState t) (MaybeT m) a)
type Unifier t = UnifierT t Identity

unwrapUnifierT (UnifierT stateM) = stateM

emptyUnifier :: Monad m => UnifierT t m ()
emptyUnifier = UnifierT $ put emptyState

freshVar :: Monad m => UnifierT t m Var
freshVar = UnifierT $ do
  id <- gets nextId
  modify $ \s -> s {nextId = id + 1}
  pure id

unify :: Constraint t -> Solution t -> Maybe (Solution t)
unify = undefined

unifySolutions :: Solution t -> Solution t -> Maybe (Solution t)
unifySolutions sol = foldr ((=<<) . unify) (Just sol) . solToConstrs
  where
    solToConstrs sol = mapFst varToT <$> Map.toList sol
    mapFst f (x, y) = (f x, y)
    varToT :: Var -> t
    varToT = undefined

liftMaybe :: Monad m => Maybe a -> MaybeT m a
liftMaybe = MaybeT . pure

addConstraint :: Monad m => Constraint t -> UnifierT t m ()
addConstraint c = UnifierT $ do
  state <- get
  void $ lift $ do
    newSol <- liftMaybe $ unify c $ solution state
    pure $ state {solution = newSol}

solveT :: Monad m => UnifyState t -> UnifierT t m a -> MaybeT m (Solution t)
solveT initState (UnifierT stateM) = solution <$> (execStateT stateM initState)

solve :: Unifier t a -> Maybe (Solution t)
solve = runIdentity . runMaybeT . solveT emptyState


instance Monad m => Functor (UnifierT t m) where
  fmap f (UnifierT ma) = UnifierT $ fmap f ma

instance Monad m => Applicative (UnifierT t m) where
  pure = UnifierT . pure
  UnifierT mf <*> UnifierT mx = UnifierT $ do
    f <- mf
    UnifyState {nextId = id1, solution = sol1} <- get
    x <- mx
    UnifyState {nextId = id2, solution = sol2} <- get
    lift $ do
      newSol <- liftMaybe $ unifySolutions sol1 sol2
      pure $ UnifyState {nextId = max id1 id2, solution = newSol}
    pure $ f x

instance Monad m => Monad (UnifierT t m) where
  UnifierT ma >>= f = UnifierT $ do
    a <- ma
    UnifyState {nextId = id1, solution = sol1} <- get
    b <- unwrapUnifierT $ f a
    UnifyState {nextId = id2, solution = sol2} <- get
    lift $ do
      newSol <- liftMaybe $ unifySolutions sol1 sol2
      pure $ UnifyState {nextId = max id1 id2, solution = newSol}
    pure b
