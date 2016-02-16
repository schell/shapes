{-# LANGUAGE PatternSynonyms, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

module Main where

import Control.Monad
import Control.Monad.State.Strict
import Control.Lens
import Criterion.Main
import GHC.Word
import Linear.Epsilon
import Linear.V2
import Physics.Broadphase
import Physics.Constraint
import Physics.ConstraintSolver
import Physics.ContactSolver
import Physics.Contact
import Physics.Object
import qualified Physics.Solvers as S
import Physics.Transform
import Physics.World hiding (testWorld)
import Physics.WorldSolver
import Utils.Utils
import Debug.Trace

import Physics.Scenes.Scene
import Physics.Scenes.Stacks

import qualified Physics.BenchGeometry as BG
import qualified BenchLinear as BL

updateWorld :: (Epsilon n, Floating n, Ord n)
            => Scene n (WorldObj n)
            -> n
            -> S.ContactSolverWorldStateM n (WorldObj n) ()
updateWorld scene dt = do
  _1 %= applyExternals (scene ^. scExts) dt
  ks <- fmap culledKeys $ use _1
  wsolve' S.contactSolver' worldChanged maxSolverIterations ks worldPair dt
  _1 %= advanceWorld dt
  _1.worldObjs %= fmap updateShape
  where maxSolverIterations = 3
        worldChanged = const . const $ True

stepWorld' :: Int -> S.ContactSolverWorldStateM Double (WorldObj Double) ()
stepWorld' 0 = return ()
stepWorld' !x = updateWorld scene'' 0.01 >> stepWorld' (x - 1)

stepWorld :: Int
           -> S.ContactSolverWorldState Double (WorldObj Double)
           -> S.ContactSolverWorldState Double (WorldObj Double)
stepWorld n state0 = snd $ runState (stepWorld' n) state0

initialState :: S.ContactSolverWorldState Double (WorldObj Double)
initialState =
  (scene ^. scWorld, S.emptyContactSolverState (scene ^. scContactBeh))
  where scene = scene''' :: Scene Double (WorldObj Double)

initialStateOpt :: S.ContactSolverWorldState Double (WorldObj Double)
initialStateOpt =
  (scene ^. scWorld, S.emptyOptContactSolverState (scene ^. scContactBeh))
  where scene = scene''' :: Scene Double (WorldObj Double)

main :: IO ()
-- 1 frame: 7.2ms (is this a misevaluation due to laziness?)
-- 10 frames: 219ms
-- 100 frames: 2.0s
-- 400 frames: 8.2s
main = defaultMain [ bench "updateWorld 10" $ whnf (show . fst . stepWorld 10) s0 ]
  where s0 = stepWorld 10 initialState
--main = do
  --(x, y) <- return $ stepWorld 200 initialStateOpt
  --return ()
--main = BG.main
