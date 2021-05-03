{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module GameState (
  appLoop,
  initialWorld,
  clamp,
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


data World = World
  { player :: V2 Float
  , opponent :: V2 Float
  , ticksLastFrame :: Word32
  , playerControl :: PlayerControl
  , opponentControl :: PlayerControl
  , projectilePosition :: V2 Float
  , projectileDirection :: V2 Float
  , playerScore :: Integer
  , opponentScore :: Integer
  }
  deriving (Show)

data PlayerControl = PlayerControl
  { moveLeft :: SDL.InputMotion
  , moveRight :: SDL.InputMotion
  , moveUp :: SDL.InputMotion
  , moveDown :: SDL.InputMotion
  }
  deriving (Show)

initialWorld :: World
initialWorld =
  World
    { player = playerStartPosition
    , opponent = opponentStartPosition
    , ticksLastFrame = 0
    , playerControl = PlayerControl SDL.Released SDL.Released SDL.Released SDL.Released
    , opponentControl = PlayerControl SDL.Released SDL.Released SDL.Released SDL.Released
    , projectilePosition = V2 320 240 -- TODO: Randomize y position
    , projectileDirection = V2 (-1) 1 -- TODO: Randomize Direction
    , playerScore = 0
    , opponentScore = 0
    }

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

drawWorld :: MonadIO m => Font -> SDL.Renderer -> World -> m ()
drawWorld font renderer world = do
  let position = round <$> player world
  -- SDL.rendererDrawColor renderer SDL.$= V4 0 255 255 255
  SDL.rendererDrawColor renderer SDL.$= white
  let rect = SDL.Rectangle (SDL.P position) rectangleSize
  SDL.drawRect renderer $ Just rect

  drawScore font renderer world

  drawOpponent renderer world

  drawCenterLine renderer

  drawProjectile renderer world

drawOpponent :: MonadIO m => SDL.Renderer -> World -> m ()
drawOpponent renderer w = do
  let rect = SDL.Rectangle (SDL.P position) rectangleSize
  SDL.drawRect renderer $ Just rect
 where
  position = round <$> opponent w

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

drawScore :: MonadIO m => Font -> SDL.Renderer -> World -> m ()
drawScore font r w = do
  fontSurface <- SDL.Font.blended font white (T.pack $ show (playerScore w) <> "   " <> show (opponentScore w))
  scoreSprite <- toTexture fontSurface
  SDL.copyEx r scoreSprite Nothing (Just $ floor <$> C.mkRect 270 10 100 50) 0.0 Nothing (V2 False False)
    where
      toTexture surface = SDL.createTextureFromSurface r surface

drawCenterLine :: MonadIO m => SDL.Renderer -> m ()
drawCenterLine renderer =
  SDL.drawLine renderer bottomMid topMid
 where
  bottomMid = SDL.P $ V2 320 0
  topMid = SDL.P $ V2 320 480

drawProjectile :: MonadIO m => SDL.Renderer -> World -> m ()
drawProjectile renderer world = do
  let projectile = SDL.Rectangle (SDL.P worldProjectilePosition) projectileSize
  SDL.fillRect renderer $ Just projectile
  SDL.drawRect renderer $ Just projectile
 where
  worldProjectilePosition = round <$> projectilePosition world

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
