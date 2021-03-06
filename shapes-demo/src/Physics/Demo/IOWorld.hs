{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Physics.Demo.IOWorld where

import Control.Monad
import Control.Monad.IO.Class
import Control.Lens ((^.), (.~), (%~), (&), makeLenses)
import Data.Proxy


import Linear.V2
import Linear.Matrix (M33)

import EasySDL.Draw
import qualified SDL.Event as E
import qualified SDL.Time as T
import qualified SDL.Video.Renderer as R
import qualified SDL.Input.Keyboard as K
import qualified SDL.Input.Keyboard.Codes as KC
import GameLoop hiding (testStep)

import Physics.Draw
import Physics.Draw.Canonical
import Physics.Engine.Class
import Utils.Utils

import Physics.Scenes.Scene
import Physics.Demo.Scenes

class (PhysicsEngine e, PEExternalObj e ~ (), MonadIO (DemoM e)) => Demo e where
  type DemoM e :: * -> *
  runDemo :: Proxy e -> Scene e -> DemoM e a -> IO a
  resetEngine :: Proxy e -> Scene e -> DemoM e ()
  drawWorld :: Proxy e -> R.Renderer -> M33 Double -> DemoM e ()
  demoWorld :: Proxy e -> DemoM e (PEWorld' e)
  worldContacts :: Proxy e -> DemoM e [Contact]
  worldAabbs :: Proxy e -> DemoM e [Aabb]
  debugEngineState :: Proxy e -> DemoM e String
  updateWorld :: Proxy e -> DemoM e ()

data DemoState =
  DemoState { _demoFinished :: Bool
            , _demoSceneIndex :: Int
            , _demoDrawDebug :: Bool
            , _demoPrintDebug :: Bool
            , _demoViewTransform :: M33 Double
            }
makeLenses ''DemoState

getViewTransform :: V2 Double -> V2 Double -> M33 Double
getViewTransform window scale = fst $ viewTransform window scale (V2 0 0)

initialState :: Int -> M33 Double -> DemoState
initialState i = DemoState False i True False

nextInitialState :: (Demo e) => Proxy e -> DemoState -> Int -> DemoM e DemoState
nextInitialState p DemoState{..} i = do
  resetEngine p (scenes p !! i)
  return $ initialState i _demoViewTransform
    & demoDrawDebug .~ _demoDrawDebug
    & demoPrintDebug .~ _demoPrintDebug

timeStep :: Num a => a
timeStep = 10

renderWorld :: (Demo e) => Proxy e -> R.Renderer -> DemoState -> DemoM e ()
renderWorld p r DemoState{..} = do
  liftIO $ setColor r black
  drawWorld p r _demoViewTransform

renderContacts :: (Demo e) => Proxy e -> R.Renderer -> DemoState -> DemoM e ()
renderContacts p r DemoState{..} = do
  liftIO $ setColor r pink
  contacts <- worldContacts p
  liftIO $ mapM_ (drawContact r . transform _demoViewTransform) contacts

renderAabbs :: (Demo e) => Proxy e -> R.Renderer -> DemoState -> DemoM e ()
renderAabbs p r DemoState{..} = do
  liftIO $ setColor r silver
  aabbs <- worldAabbs p
  liftIO $ mapM_ (drawAabb r . transform _demoViewTransform) aabbs

demoStep :: (Demo e) => Proxy e -> R.Renderer -> DemoState -> DemoM e DemoState
demoStep p r s0@DemoState{..} = do
  events <- liftIO E.pollEvents
  liftIO $ clearScreen r
  renderWorld p r s0
  when (s0 ^. demoDrawDebug) $ do
    renderContacts p r s0
    renderAabbs p r s0
  when (s0 ^. demoPrintDebug) $ do
    debug <- debugEngineState p
    liftIO $ print debug
  s1 <- foldM (handleEvent p) s0 events
  updateWorld p
  liftIO $ R.present r
  return s1

handleEvent :: (Demo e) => Proxy e -> DemoState -> E.Event -> DemoM e DemoState
handleEvent _ s0 (E.Event _ E.QuitEvent) =
  return s0 { _demoFinished = True }
handleEvent p s0 (E.Event _ (E.KeyboardEvent (E.KeyboardEventData _ motion _ key)))
  | motion == E.Pressed =
    handleKeypress p s0 (K.keysymScancode key) (K.keysymModifier key)
  | otherwise = return s0
handleEvent _ s0 _ = return s0

handleKeypress :: (Demo e)
               => Proxy e
               -> DemoState
               -> K.Scancode
               -> K.KeyModifier
               -> DemoM e DemoState
handleKeypress p state KC.ScancodeR _ =
  nextInitialState p state (state ^. demoSceneIndex)
handleKeypress p state KC.ScancodeN km
  | K.keyModifierLeftShift km || K.keyModifierRightShift km =
    nextInitialState p state $
    (state ^. demoSceneIndex - 1)
    `posMod` sceneCount
  | otherwise =
    nextInitialState p state $
    (state ^. demoSceneIndex + 1)
    `mod` sceneCount
  where sceneCount = length $ scenes p
handleKeypress _ state KC.ScancodeD _ =
  return $ state & demoDrawDebug %~ not
handleKeypress _ state KC.ScancodeP _ =
  return $ state & demoPrintDebug %~ not
handleKeypress _ state _ _ = return state

demoMain :: (Demo e) => Proxy e -> V2 Double -> V2 Double -> R.Renderer -> IO ()
demoMain p window scale r = do
  t0 <- T.ticks
  let demo = timedRunUntil t0 timeStep (initialState 0 $ getViewTransform window scale) _demoFinished (\s _ -> demoStep p r s)
  runDemo p (head $ scenes p) demo
