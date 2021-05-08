{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module GameState (
  appLoop,
  initialWorld,
  clamp,
  World,
) where

import qualified SDL
import qualified SDL.Raw.Timer as SDLTimer

import Constants
import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import Data.IORef
import Data.List (foldl')
import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Linear
import qualified SDL.Font (load, solid, initialize, blended)
import Common (renderSurfaceToWindow)
import qualified Common as C
import qualified Data.Text as T
import SDL.Font (Font(Font))

import Renderer
import State

appLoop :: Font -> SDL.Renderer -> IORef World -> IO ()
appLoop font renderer worldRef = do
  events <- SDL.pollEvents
  world <- readIORef worldRef
  t <- SDLTimer.getTicks

  let timeToWait = frameTargetTime - fromIntegral (t - ticksLastFrame world)
      qPressed = any eventIsQPress events
      timeToWait' =
        if timeToWait > 0 && timeToWait < frameTargetTime
          then timeToWait
          else 0

  SDL.delay $ floor timeToWait'
  t' <- SDLTimer.getTicks

  let world' = updateWorld events world t'

  print world'
  render font renderer world'
  writeIORef worldRef world'
  unless qPressed (appLoop font renderer worldRef)

updateControl :: SDL.Event -> PlayerControl -> PlayerControl
updateControl event = case SDL.eventPayload event of
  SDL.KeyboardEvent e ->
    let motion = SDL.keyboardEventKeyMotion e
     in case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
          SDL.KeycodeH -> \c -> c{moveLeft = motion}
          SDL.KeycodeL -> \c -> c{moveRight = motion}
          SDL.KeycodeJ -> \c -> c{moveDown = motion}
          SDL.KeycodeK -> \c -> c{moveUp = motion}
          _ -> id
  _ -> id

updateOpponentControl :: SDL.Event -> PlayerControl -> PlayerControl
updateOpponentControl event = case SDL.eventPayload event of
  SDL.KeyboardEvent e ->
    let motion = SDL.keyboardEventKeyMotion e
     in case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
          SDL.KeycodeA -> \c -> c{moveLeft = motion}
          SDL.KeycodeD -> \c -> c{moveRight = motion}
          SDL.KeycodeS -> \c -> c{moveDown = motion}
          SDL.KeycodeW -> \c -> c{moveUp = motion}
          _ -> id
  _ -> id

updateWorld :: [SDL.Event] -> World -> Word32 -> World
updateWorld events world t
     | leftWallCollision (projectilePosition world) =
        initialWorld { opponentScore = opponentScore' + 1
                     , playerScore = playerScore' }
     | rightWallCollision (projectilePosition world) = 
        initialWorld { opponentScore = opponentScore'
                     , playerScore = playerScore' + 1 }
     | otherwise = 
          World
            { player = clampedPlayer
            , opponent = clampedOpponent
            , ticksLastFrame = t
            , playerControl = playerControl'
            , opponentControl = opponentControl'
            , projectilePosition = projectilePosition world + 2 * worldProjectile'
            , projectileDirection = worldProjectile'
            , playerScore = playerScore'
            , opponentScore = opponentScore'
            }
              where
                deltaTime = fromIntegral (t - ticksLastFrame world) / 1000
                deltaPos = deltaTime * 200
                playerControl' = foldl' (flip updateControl) (playerControl world) events
                opponentControl' :: PlayerControl
                opponentControl' = foldl' (flip updateOpponentControl) (opponentControl world) events
                player' = player world + fmap (* deltaPos) (controlToVec playerControl')
                opponent' = opponent world + fmap (* deltaPos) (controlToVec opponentControl')
                clampedPlayer = clamp player'
                clampedOpponent = clamp opponent'
                worldProjectile' = updateProjectileDirection world
                playerScore' = playerScore world
                opponentScore' = opponentScore world

rightWallCollision :: (Ord a, Num a) => V2 a -> Bool
rightWallCollision (V2 x y) = x > (640 - projectileSizeX) 
 where
  (V2 projectileSizeX _) = projectileSize

leftWallCollision :: (Ord a, Num a) => V2 a -> Bool
leftWallCollision (V2 x y) = x < 0
 where
  (V2 projectileSizeX _) = projectileSize

controlToVec :: PlayerControl -> V2 Float
controlToVec pc =
  let x_left = if SDL.Pressed == moveLeft pc then (-1) else 0
      x_right = if SDL.Pressed == moveRight pc then 1 else 0
      y_up = if SDL.Pressed == moveUp pc then (-1) else 0
      y_down = if SDL.Pressed == moveDown pc then 1 else 0
      x = x_left + x_right
      y = y_up + y_down
   in V2 x y


eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
  case SDL.eventPayload event of
    SDL.KeyboardEvent keyboardEvent ->
      SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
        && SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
    _ -> False

render :: (MonadIO m) => Font -> SDL.Renderer -> World -> m ()
render font renderer world = do
  -- Set background color
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255

  -- Clear the back buffer
  SDL.clear renderer

  drawWorld font renderer world

  -- Swap front and back buffers
  SDL.present renderer

clamp :: (Num a, Ord a) => V2 a -> V2 a
clamp = liftA2 min ub . liftA2 max lb


updateProjectileDirection :: World -> V2 Float
updateProjectileDirection w
  | playerCollision (player w) (projectilePosition w) = V2 (abs dx) dy
  | opponentCollision opponentPosition (projectilePosition w) = V2 (- abs dx) dy
  | y > 480 - projectileSizeY || y < 0 = V2 dx (- dy)
  | otherwise = V2 dx dy
 where
  (V2 x y) = projectilePosition w
  (V2 dx dy) = projectileDirection w
  (V2 _ projectileSizeY) = projectileSize
  opponentPosition = opponent w

playerCollision :: (Ord a, Num a) => V2 a -> V2 a -> Bool
playerCollision (V2 x y) (V2 px py) =
  py > y && py < y + playerSizeY -- within paddle height
    && px < x + playerSizeX && px > x
 where
  (V2 playerSizeX playerSizeY) = rectangleSize

opponentCollision :: (Ord a, Num a) => V2 a -> V2 a -> Bool
opponentCollision (V2 x y) (V2 px py) =
  (py > y) && (py < y + playerSizeY) 
    && px' < x + playerSizeX && px' > x
 where
  px' = px + projectileSizeX
  (V2 playerSizeX playerSizeY) = rectangleSize
  (V2 projectileSizeX _) = projectileSize
