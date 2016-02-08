{-# LANGUAGE PatternSynonyms, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, BangPatterns #-}

module Main where

import Control.Monad
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

data SP a b = SP !a !b
type EngineState n = SP (World (WorldObj n)) (S.ContactSolverState n (WorldObj n))

fst' :: SP a b -> a
fst' (SP x _) = x

updateWorld :: (Epsilon n, Floating n, Ord n) => Scene n (WorldObj n) -> n -> EngineState n -> EngineState n
updateWorld scene dt (SP w s) = SP w'' s'
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 3
        worldChanged = const . const $ True
        ks = culledKeys w1
        (w', s') = wsolve' S.contactSolver' worldChanged maxSolverIterations ks worldPair w1 dt s
        w'' = advanceWorld dt w'

stepWorld :: Int -> EngineState Double -> EngineState Double
stepWorld 0 !s = s
stepWorld !x !s = stepWorld (x - 1) $ updateWorld scene'' 0.01 s

initialState :: EngineState Double
initialState =
  SP (scene ^. scWorld) (S.emptyContactSolverState (scene ^. scContactBeh))
  where scene = scene''' :: Scene Double (WorldObj Double)

main :: IO ()
--main = defaultMain [ bench "updateWorld" $ whnf (show . fst' . stepWorld 1) initialState] -- 23.2ms
--main = do
  --(SP x y) <- return $ stepWorld 200 initialState
  --return ()
main = BG.main
