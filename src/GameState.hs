{-# LANGUAGE OverloadedStrings #-}
module GameState
  ( appLoop
  , initialWorld
  ) where


import qualified SDL
import qualified SDL.Raw.Timer          as SDLTimer

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef
import           Data.List              (foldl')
import           Data.Word              (Word32)
import           Foreign.C.Types        (CInt)
import           Linear

data World = World
  { player :: V2 Float
  , ticksLastFrame :: Word32
  , playerControl :: PlayerControl
  } deriving Show

data PlayerControl = PlayerControl 
  { moveLeft :: SDL.InputMotion
  , moveRight :: SDL.InputMotion
  , moveUp :: SDL.InputMotion
  , moveDown :: SDL.InputMotion
  } deriving Show

rectangleSize :: V2 CInt
rectangleSize = V2 16 20

framesPerSecond :: Float
framesPerSecond = 60

frameTargetTime :: Float
frameTargetTime = 1000 / framesPerSecond;

initialWorld :: World
initialWorld = World { player = V2 6 10
                     , ticksLastFrame = 0
                     , playerControl = PlayerControl SDL.Released SDL.Released SDL.Released SDL.Released
                     }


appLoop :: SDL.Renderer -> IORef World -> IO ()
appLoop renderer worldRef = do
  events <- SDL.pollEvents
  world <- readIORef worldRef
  t <- SDLTimer.getTicks

  let timeToWait = frameTargetTime - fromIntegral (t - ticksLastFrame world)
      qPressed = any eventIsQPress events
      timeToWait' = if timeToWait > 0 && timeToWait < frameTargetTime
         then timeToWait
         else 0

  SDL.delay $ floor timeToWait'
  t' <- SDLTimer.getTicks

  let world' = updateWorld events world t'

  print world'
  render renderer world'
  writeIORef worldRef world'
  unless qPressed (appLoop renderer worldRef)

updateControl :: SDL.Event -> PlayerControl -> PlayerControl
updateControl event = case SDL.eventPayload event of
  SDL.KeyboardEvent e ->
    let motion = SDL.keyboardEventKeyMotion e
     in case SDL.keysymKeycode $ SDL.keyboardEventKeysym e of
          SDL.KeycodeH -> \c -> c { moveLeft = motion }
          SDL.KeycodeL -> \c -> c { moveRight = motion }
          SDL.KeycodeJ -> \c -> c { moveDown = motion }
          SDL.KeycodeK -> \c -> c { moveUp = motion }
          _ -> id
  _ -> id
  

updateWorld :: [SDL.Event] -> World -> Word32 -> World
updateWorld events world t =
  let deltaTime = fromIntegral (t - ticksLastFrame world) / 1000
      deltaPos = deltaTime * 200
      playerControl' = foldl' (flip updateControl) (playerControl world) events
  in
  World { player = player world + fmap (*deltaPos) (controlToVec playerControl')
        , ticksLastFrame = t
        , playerControl = playerControl'
        }

controlToVec :: PlayerControl -> V2 Float
controlToVec pc =
  let x_left = if SDL.Pressed == moveLeft pc then (-1) else 0
      x_right = if SDL.Pressed == moveRight pc then 1 else 0
      y_up = if SDL.Pressed == moveUp pc then (-1) else 0
      y_down = if SDL.Pressed == moveDown pc then 1 else 0
      x = x_left + x_right
      y = y_up + y_down
   in V2 x y

drawWorld :: MonadIO m => SDL.Renderer -> World -> m ()
drawWorld renderer world = do
  let position = round <$> player world
  SDL.rendererDrawColor renderer SDL.$= V4 0 255 255 255
  let rect = SDL.Rectangle (SDL.P position) rectangleSize
  SDL.drawRect renderer $ Just rect

eventIsQPress :: SDL.Event -> Bool
eventIsQPress event =
    case SDL.eventPayload event of
      SDL.KeyboardEvent keyboardEvent ->
        SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
        SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
      _ -> False


render :: (MonadIO m) => SDL.Renderer -> World -> m ()
render renderer world = do
  -- Set background color
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255

  -- Clear the back buffer
  SDL.clear renderer

  drawWorld renderer world
  
  -- Swap front and back buffers
  SDL.present renderer
