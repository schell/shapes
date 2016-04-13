{-# LANGUAGE TypeFamilies #-}

module Physics.Demo.OptWorld where

import Control.Lens
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector.Unboxed as V
import Data.Maybe

import qualified Physics.Broadphase.Aabb as B
import Physics.Contact
import Physics.Contact.ConvexHull
import Physics.Engine
import qualified Physics.Engine.Main as OM
import Physics.World.Class

import Physics.Draw.Canonical
import qualified Physics.Draw.Opt as D
import Physics.Demo.IOWorld (Demo(..))

import Utils.Descending
import Utils.Utils

instance Demo Engine where
  type DemoM Engine = StateT (OM.EngineState RealWorld) IO
  runDemo _ scene action = do
    eState <- liftIO . stToIO $ OM.initEngine scene
    evalStateT action eState
  resetEngine _ scene =
    convertEngineT $ OM.changeScene scene
  drawWorld p r vt = do
    world <- demoWorld p
    liftIO $ D.drawWorld r vt world
  demoWorld _ = view _1 <$> get
  worldContacts p = do
    world <- demoWorld p
    let cs :: Descending Contact'
        cs = fmap (flipExtractUnsafe . snd) . join $ generateContacts <$> culledPairs
        pairKeys = B.culledKeys world
        culledPairs = fmap f pairKeys
        f :: (Int, Int) -> (ConvexHull, ConvexHull)
        f ij = fromJust $ iixView (\k -> wObj k . woShape) ij world
    return . _descList $ toCanonical <$> cs
  worldAabbs p = do
    world <- demoWorld p
    return $ toCanonical . snd <$> V.toList (B.toAabbs world)
  debugEngineState _ = return "<insert debug trace here>"
  updateWorld _ = void . convertEngineT . OM.updateWorld

convertEngineT :: OM.EngineT RealWorld a -> DemoM Engine a
convertEngineT action =
  StateT $ stToIO . runStateT action