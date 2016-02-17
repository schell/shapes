{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module WorldSolverST where

import Physics.WorldSolver

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.ST
import qualified Data.HashTable.ST.Linear as H
import Data.Maybe
import Physics.PairMap (Key)
import Physics.World (World(..))
import Utils.Utils
import Physics.ConstraintSolver (PairCacheInitializer, PairUpdater, WorldCacheInitializer)

type HashTable s k v = H.HashTable s k v

data WorldSolverState s pairCache worldCache =
  WorldSolverState { _wssPairCaches :: !(HashTable s Key pairCache)
                   , _wssWorldCache :: !worldCache
                   } deriving (Show)
makeLenses ''WorldSolverState

type WorldState worldObj solverState = SP (World worldObj) solverState

type WorldStateM s worldObj solverState =
  StateT (WorldState worldObj solverState) (ST s)

type WorldStateM' s worldObj pairCache worldCache =
  WorldStateM s worldObj (WorldSolverState s pairCache worldCache)

initOnePairCache :: PairCacheInitializer a c wc
                 -> Key
                 -> WorldLens Key (World a) (a, a)
                 -> WorldStateM' s a c wc ()
initOnePairCache pairCacheInit pairKey l = do
  SP world (WorldSolverState pairCaches worldCache) <- get
  -- calculate updated pairCache and worldCache
  srcPairCache <- lift $ H.lookup pairCaches pairKey
  -- TODO: figure out a way to get rid of these fromJusts
  let ab = fromJust $ world ^? l pairKey
      (pairCache, worldCache') = pairCacheInit pairKey ab srcPairCache worldCache
  -- insert updated pairCache
  case pairCache of Just pc -> lift $ H.insert pairCaches pairKey pc
                    Nothing -> return ()
  -- insert updated worldCache
  spSnd.wssWorldCache .= worldCache'

initConstraintSolverState :: PairCacheInitializer a c wc
                          -> WorldCacheInitializer a x wc
                          -> WSGen (World a) Key (a, a) x (WorldStateM' s a c wc)
initConstraintSolverState pairCacheInit worldCacheInit pairKeys l dt = do
  SP world solverState <- get
  spSnd.wssWorldCache %= worldCacheInit pairKeys l world dt
  mapM_ (\pairKey -> initOnePairCache pairCacheInit pairKey l) pairKeys

improveOne :: PairUpdater a c wc
           -> Key
           -> WorldLens Key (World a) (a, a)
           -> WorldStateM' s a c wc ()
improveOne pairUpdater pairKey l = do
  SP world (WorldSolverState pairCaches worldCache) <- get
  -- TODO: figure out a way to get rid of these fromJusts
  Just pairCache <- lift $ H.lookup pairCaches pairKey
  let ab = fromJust $ world ^? l pairKey
      (ab', pairCache', worldCache') = pairUpdater pairKey ab pairCache worldCache
  -- TODO: find a better way to do these state updates
  spFst.l pairKey .= ab'
  lift $ H.insert pairCaches pairKey pairCache'
  spSnd.wssWorldCache .= worldCache'

improve :: PairUpdater a c wc -> WSFunc (World a) Key (a, a) (WorldStateM' s a c wc)
improve pairUpdater l = do
  (world0, csState0) <- get
  let pairKeys = keys (csState0 ^. csPairCaches)
  put $ foldl' f (world0, csState0) pairKeys
  where f (world, csState) pairKey =
          improveOne pairUpdater pairKey l world csState
