{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Physics.WorldSolverST where

import Physics.WorldSolver

import Control.Lens
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Monad.ST
import qualified Data.HashTable.ST.Linear as H
import qualified Data.HashTable.Class as HC
import Data.Maybe
import Physics.PairMap (Key)
import Physics.World (World(..))
import Utils.Utils

-- TODO: Put HashTable and dt into ReaderT, World into StateT, ST as base monad

type HashTable s k v = H.HashTable s k v

data WorldState worldObj worldCache =
  WorldState { _wsWorld :: World worldObj
             , _wsCache :: worldCache
             } deriving (Show)
makeLenses ''WorldState

type WorldM s worldObj pairCache worldCache =
  StateT (WorldState worldObj worldCache) (ReaderT (HashTable s Key pairCache) (ST s))

prepareNextFramePairCache :: (Key -> Pair wObj -> Maybe pCache -> wCache -> (Maybe pCache, wCache))
                          -> Key
                          -> Pair wObj
                          -> WorldM s wObj pCache wCache ()
prepareNextFramePairCache stepWorldCache pairKey pair = do
  pCaches <- ask
  pCache <- lift . lift $ H.lookup pCaches pairKey
  wCache <- use wsCache
  let (pCache', wCache') = stepWorldCache pairKey pair pCache wCache
  wsCache .= wCache'
  maybe (return ()) (lift . lift . (H.insert pCaches pairKey)) pCache'

prepareNextFrame :: (x -> wCache -> wCache)
                 -> (Key -> Pair wObj -> Maybe pCache -> wCache -> (Maybe pCache, wCache))
                 -> x
                 -> [(Key, Pair wObj)]
                 -> WorldM s wObj pCache wCache ()
prepareNextFrame stepWorldCache stepPairCache dt keyedPairs = do
  wsCache %= stepWorldCache dt
  mapM_ (uncurry $ prepareNextFramePairCache stepPairCache) keyedPairs

improveOne :: (Key -> Pair wObj -> pCache -> wCache -> (Pair wObj, pCache, wCache))
           -> WorldLens Key (World wObj) (Pair wObj)
           -> Key
           -> Pair wObj
           -> pCache
           -> WorldM s wObj pCache wCache ()
improveOne solvePair l pairKey pair pCache = do
  WorldState world wCache <- get
  let (pair', pCache', wCache') = solvePair pairKey pair pCache wCache
      world' = world & l pairKey .~ pair'
  put $ WorldState world' wCache'
  pCaches <- ask
  lift . lift $ H.insert pCaches pairKey pCache'

improve :: (Key -> Pair wObj -> pCache -> wCache -> (Pair wObj, pCache, wCache))
        -> WorldLens Key (World wObj) (Pair wObj)
        -> WorldM s wObj pCache wCache ()
improve solvePair l = do
  pCaches <- ask
  keyedPCaches <- lift . lift $ HC.toList pCaches
  world <- use wsWorld
  -- TODO: is there a way to get rid of this fromJust?
  let keyPairPCaches = fmap (\(k, pc) -> (k, fromJust $ world ^? l k, pc)) keyedPCaches
  mapM_ (\(k, p, pc) -> improveOne solvePair l k p pc) keyPairPCaches
