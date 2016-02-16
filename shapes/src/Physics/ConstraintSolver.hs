{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Physics.ConstraintSolver where

import Control.Lens
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Maybe
import Physics.Constraint
import Physics.PairMap
import Physics.World hiding (solveOne, solveMany)
import Physics.WorldSolver
import Utils.Utils

{-
n/x = dt (number type)
a = object
c = pairwise cache
wc = world cache
w = world
-}

-- (was: State)
data ConstraintSolverState c wc =
  ConstraintSolverState { _csPairCaches :: !(PairMap c)
                        , _csWorldCache :: !wc
                        } deriving (Show, Eq)
makeLenses ''ConstraintSolverState

type WorldState worldObj solverState = (World worldObj, solverState)
type WorldStateM worldObj solverState = State (WorldState worldObj solverState)

type WorldStateM' a c wc = WorldStateM a (ConstraintSolverState c wc)

-- initialize solver cache for a pair of objects (may affect global cache)
-- (was: Generator)
type PairCacheInitializer a c wc = Key -> (a, a) -> Maybe c -> wc -> (Maybe c, wc)

-- use caches to update objects (caches are updated as well)
-- (was: Applicator)
type PairUpdater a c wc = Key -> (a, a) -> c -> wc -> ((a, a), c, wc)

-- initialize global solver cache
--type WorldCacheInitializer a x wc = WSGen (World a) Key (a, a) x (State (World a, wc))
type WorldCacheInitializer a x wc = [Key] -> WorldLens Key (World a) (a, a) -> World a -> x -> wc -> wc

initOnePairCache :: PairCacheInitializer a c wc
                 -> Key
                 -> WorldLens Key (World a) (a, a)
                 -> World a
                 -> PairMap c
                 -> ConstraintSolverState c wc
                 -> ConstraintSolverState c wc
initOnePairCache pairCacheInit pairKey l world pairCachesSrc csStateAccum =
  csStateAccum & csPairCaches %~ setPairCache & csWorldCache .~ worldCache
  where ab = fromJust $ world ^? l pairKey
        (pairCache, worldCache) = pairCacheInit pairKey ab
                                   (lookupPair pairKey pairCachesSrc)
                                   (csStateAccum ^. csWorldCache)
        setPairCache = maybe id (insertPair pairKey) pairCache

initConstraintSolverState :: PairCacheInitializer a c wc
                          -> WorldCacheInitializer a x wc
                          -> WSGen (World a) Key (a, a) x (WorldStateM' a c wc)
initConstraintSolverState pairCacheInit worldCacheInit pairKeys l dt = do
  (world, csState0) <- get
  let worldCache1 = worldCacheInit pairKeys l world dt (csState0 ^. csWorldCache)
      csState1 = foldl' f (ConstraintSolverState IM.empty worldCache1) pairKeys
      pairCaches0 = csState0 ^. csPairCaches -- we may need to copy from the previous frame's pairCaches
      f !csState !pairKey = initOnePairCache pairCacheInit pairKey l world pairCaches0 csState
  _2 .= csState1

improveOne :: PairUpdater a c wc
           -> Key
           -> WorldLens Key (World a) (a, a)
           -> World a
           -> ConstraintSolverState c wc
           -> (World a, ConstraintSolverState c wc)
improveOne pairUpdater pairKey l world csState =
  (world', csState')
  where ab = fromJust $ world ^? l pairKey
        worldCache = csState ^. csWorldCache
        pairCache = fromJust $ lookupPair pairKey (csState ^. csPairCaches)
        (ab', pairCache', worldCache') = pairUpdater pairKey ab pairCache worldCache
        world' = world & l pairKey .~ ab'
        csState' = csState & csPairCaches %~ insertPair pairKey pairCache'
                   & csWorldCache .~ worldCache'

improve :: PairUpdater a c wc -> WSFunc (World a) Key (a, a) (WorldStateM' a c wc)
improve pairUpdater l = do
  (world0, csState0) <- get
  let pairKeys = keys (csState0 ^. csPairCaches)
  put $ foldl' f (world0, csState0) pairKeys
  where f (world, csState) pairKey =
          improveOne pairUpdater pairKey l world csState
