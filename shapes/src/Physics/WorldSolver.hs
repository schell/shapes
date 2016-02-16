{-# LANGUAGE FlexibleInstances, FlexibleContexts, RankNTypes #-}

module Physics.WorldSolver where

import Control.Lens
import Control.Monad.ST
import Control.Monad.State.Class

{-
k = key (object index)
w = world (contains objects)
a = object type
x = timestep
s = solver state (cache)
-}
type WorldLens k w a = k -> Traversal' w a
-- WSGen :: generate new solver state (update cache)
type WSGen w k a x m = [k] -> WorldLens k w a -> x -> m ()
-- WSFunc :: update world & solver state
type WSFunc w k a m = WorldLens k w a -> m ()
-- WSChanged :: did the world actually change? (threshold for early exit)
type WSChanged w = w -> w -> Bool
-- WSolver :: everything you need to step the world
type WSolver w k a x m = (WSGen w k a x m, WSFunc w k a m)
-- WSolver' :: use one WSFunc to seed the first solver iteration, use the other for subsequent iterations (seed & improve)
type WSolver' w k a x m = (WSGen w k a x m, WSFunc w k a m, WSFunc w k a m)

-- run func to improve solution until unchanged (max n iterations)
wsimprove :: (MonadState (w, s) m) => WSFunc w k a m -> WSChanged w -> Int -> WorldLens k w a -> m ()
wsimprove _ _       0 _ = return ()
wsimprove f changed n l = do
  (w, s) <- get
  f l
  (w', s') <- get
  if changed w w'
    then wsimprove f changed (n - 1) l
    else return ()

-- apply first solver function (WSFunc) to seed the solution
-- iteratively apply second solver function to improve the solution
wsimprove' :: (MonadState (w, s) m) => WSFunc w k a m -> WSFunc w k a m -> WSChanged w -> Int -> WorldLens k w a -> m ()
wsimprove' f0 f1 changed n l = f0 l >> wsimprove f1 changed n l

-- run gen to reset solver state
-- iteratively run func to improve solution (until unchanged; up to n times)
wsolve :: (MonadState (w, s) m) => WSGen w k a x m -> WSFunc w k a m -> WSChanged w -> Int -> [k] -> WorldLens k w a -> x -> m ()
wsolve g f changed n ks l x = g ks l x >> wsimprove f changed n l

-- use WSGen to fill the solver cache before solving with wsimprove'
wsolve' :: (MonadState (w, s) m) => WSolver' w k a x m -> WSChanged w -> Int -> [k] -> WorldLens k w a -> x -> m ()
wsolve' (g, f0, f) changed n ks l x = g ks l x >> wsimprove' f0 f changed n l
