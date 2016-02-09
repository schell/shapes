{-# LANGUAGE PatternSynonyms, TemplateHaskell, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}

module Physics.TestWorld where

import Control.Monad
import Control.Lens
import qualified Data.IntMap.Strict as IM
import EasySDL.Draw
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
import Physics.Draw
import Physics.DrawWorld
import Physics.World hiding (testWorld)
import Physics.WorldSolver
import GameLoop hiding (testStep)
import qualified SDL.Event as E
import qualified SDL.Time as T
import qualified SDL.Video.Renderer as R
import qualified SDL.Input.Keyboard as K
import qualified SDL.Input.Keyboard.Codes as KC
import Utils.Utils

import Physics.Scenes.Scene
import Physics.Scenes.Scenes

type EngineState n = SP (World (WorldObj n)) (S.ContactSolverState n (WorldObj n))
data TestState = TestState { _testWorldState :: EngineState Double
                           , _testFinished :: Bool
                           , _testScene :: Scene Double (WorldObj Double)
                           , _testSceneIndex :: Int
                           }
makeLenses ''TestState

updateWorld :: (Epsilon n, Floating n, Ord n) => Scene n (WorldObj n) -> n -> EngineState n -> EngineState n
updateWorld scene dt (SP w s) = SP w''' s'
  where w1 = applyExternals (scene ^. scExts) dt w
        maxSolverIterations = 1
        worldChanged = const . const $ True
        (w', s') = wsolve' S.contactSolver' worldChanged maxSolverIterations (culledKeys w1) worldPair w1 dt s
        w'' = advanceWorld dt w'
        w''' = w'' & worldObjs %~ fmap updateShape

vt :: WorldTransform Double
vt = viewTransform (V2 400 300) (V2 20 20) (V2 0 0)

initialState :: Int -> TestState
initialState i =
  TestState (SP (scene ^. scWorld)
             (S.emptyContactSolverState $ scene ^. scContactBeh))
  False scene i
  where scene = scenes !! i

timeStep :: Num a => a
timeStep = 10

renderTest :: R.Renderer -> TestState -> IO ()
renderTest r state = do
  setColor r black
  drawWorld r vt (state ^. testWorldState . spFst)

renderContacts :: R.Renderer -> TestState -> IO ()
renderContacts r s = do
  setColor r pink
  sequence_ . join $ fmap f cs
  where f (WorldPair _ fcs) = fmap g fcs
        g = drawContact' r . LocalT vt . flipExtract
        cs = fmap generateContacts <$> culledPairs (s ^. testWorldState . spFst)

renderAabbs :: R.Renderer -> TestState -> IO ()
renderAabbs r s = do
  setColor r silver
  mapM_ (drawAabb r . LocalT vt) $ IM.elems (toAabbs world)
  where world = s ^.testWorldState.spFst

testStep :: R.Renderer -> TestState -> Word32 -> IO TestState
testStep r s0 _ = do
  events <- E.pollEvents
  withBlankScreen r $ (do
                          renderTest r s0
                          --renderContacts r s0
                          --renderAabbs r s0
                      )
  --print $ (_testWorldState s0) & spSnd %~ S.toShowableSolverState
  let s = foldl handleEvent s0 events & testWorldState %~ (updateWorld scene dt)
  return s
  where dt = fromIntegral timeStep / 1000
        scene = s0 ^. testScene

handleEvent :: TestState -> E.Event -> TestState
handleEvent s0 (E.Event _ E.QuitEvent) = s0 { _testFinished = True }
handleEvent s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed = handleKeypress s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = s0
handleEvent s0 _ = s0

handleKeypress :: TestState -> K.Scancode -> K.KeyModifier -> TestState
handleKeypress state KC.ScancodeR _ = initialState (state ^. testSceneIndex)
handleKeypress state KC.ScancodeN km
  | K.keyModifierLeftShift km || K.keyModifierRightShift km =
    initialState ((state ^. testSceneIndex - 1)
                  `posMod` length (scenes :: [Scene Double (WorldObj Double)]))
  | otherwise = initialState ((state ^. testSceneIndex + 1) `mod` length (scenes :: [Scene Double (WorldObj Double)]))
handleKeypress state _ _ = state

testMain :: R.Renderer -> IO ()
testMain r = do
  t0 <- T.ticks
  timedRunUntil t0 timeStep (initialState 0) _testFinished (testStep r)
