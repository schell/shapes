{-# LANGUAGE PatternSynonyms #-}

module Game where

import qualified Graphics.UI.SDL.Video as SDL.V
import qualified Graphics.UI.SDL.Enum as SDL.E
import qualified Graphics.UI.SDL.Types as SDL.T
import qualified Graphics.UI.SDL.Basic as SDL.B
import qualified Graphics.UI.SDL.Timer as SDL.Timer
import qualified Graphics.UI.SDL.Event as SDL.Event
import Control.Monad
import Data.Word
import Data.Maybe
import Foreign.C.Types
import GHC.Int
import SDL.Draw
import SDL.Event
import SDL.Geometry
import SDL.Loading
import Drag
import DrawTile
import SmoothSlidingGrid
import SlidingGrid
import Grid
import GameInput
import GameState
import GameTile
import Utils.Utils

data World = World { gameOver :: Bool
                   , gridDrawInfo :: GridDrawInfo CInt
                   , gridState :: GridState
                   , gridInput :: GridInput } deriving Show

drawState :: SDL.T.Renderer -> SDL.T.Rect -> [Asset] -> World -> IO ()
drawState r fullWindow assets state =
  withBlankScreen r $ drawGrid r gdi gi gs
  where gs = gridState state
        gi = gridInput state
        gdi = gridDrawInfo state

updateState :: Input -> World -> World
updateState (Just (SDL.T.QuitEvent _ _)) state = state { gameOver = True }
updateState (Just (SDL.T.KeyboardEvent evtType _ _ _ _ keysym)) state =
  if evtType == SDL.E.SDL_KEYDOWN
  then modifyState state keysym
  else state
updateState (Just (SDL.T.MouseMotionEvent { SDL.T.mouseMotionEventX = x
                                          , SDL.T.mouseMotionEventY = y })) state =
  applyMouseMotion x y state
updateState (Just (SDL.T.MouseButtonEvent { SDL.T.eventType = evtType
                                          , SDL.T.mouseButtonEventButton = button
                                          , SDL.T.mouseButtonEventX = x
                                          , SDL.T.mouseButtonEventY = y })) state =
  state { gridInput = f button x y (gridInput state) }
  where f = case evtType of
          SDL.E.SDL_MOUSEBUTTONDOWN -> applyMouseButtonDown
          SDL.E.SDL_MOUSEBUTTONUP -> applyMouseButtonUp
updateState _ state = state

modifyState :: World -> SDL.T.Keysym -> World
modifyState state keysym = case getKey keysym of
  _ -> state

applyMouseMotion :: Int32 -> Int32 -> World -> World
applyMouseMotion x y state = case gridDrag inputState of
  -- update the end of the current drag (if we are dragging)
  Nothing -> state
  Just (click, _) -> state { gridState = gs'
                           , gridInput = inputState { gridDrag = Just drag' }}
    where (drag', gs') = completelyApplyDrag drawInfo drag gs
          drawInfo = fmap fromIntegral (gridDrawInfo state)
          drag = (click, pairMap fromIntegral (x, y))
          gs = gridState state
  where inputState = gridInput state

applyMouseButtonDown :: Word8 -> Int32 -> Int32 -> GridInput -> GridInput
applyMouseButtonDown button x y state =
  if button == SDL.E.SDL_BUTTON_LEFT
  then state { gridDrag = Just (pos, pos) } -- start new drag
  else state -- we drag using the left button
  where pos = pairMap fromIntegral (x, y)

applyMouseButtonUp :: Word8 -> Int32 -> Int32 -> GridInput -> GridInput
applyMouseButtonUp button x y state =
  if button == SDL.E.SDL_BUTTON_LEFT
  then state { gridDrag = Nothing } -- release the current drag
  else state -- we drag using left button
  where pos = toGeomPointInt (x, y)

runUntilComplete :: (Monad m) => m World -> m ()
runUntilComplete game = game >>= \state -> unless (gameOver state) $ runUntilComplete game

instance Draggable (GridState) where
  setCoord coord gs@(GridState _ _ tiles) = do
    tiles' <- Grid.moveTo coord tiles
    return $ gs { gridTiles = tiles' }
  applyMove dir gs@(GridState player _ tiles) =
    if player == gridCoord tiles then gs -- TODO: actually move the player
    else gs { gridTiles = fromMaybe tiles $ slide_ dir tiles }
  checkMove dir gs@(GridState player _ tiles) =
    if player == gridCoord tiles then False -- TODO: actually see if the player can move
    else isJust (slideList dir tiles)

