{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Raw.Timer          as SDLTimer

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef
import           Data.List (foldl')
import           Foreign.C.Types        (CInt)
import           Linear
import Data.Word (Word32)

data World = World
  { player :: V2 Float
  , ticksLastFrame :: Word32
  , playerControl :: PlayerControl
  } deriving Show

data PlayerControl = PlayerControl 
  { moveLeft :: SDL.InputMotion
  , moveRight :: SDL.InputMotion
  } deriving Show

rectangleSize :: V2 CInt
rectangleSize = V2 20 20

framesPerSecond :: Float
framesPerSecond = 60

frameTargetTime :: Float
frameTargetTime = 1000 / framesPerSecond;

initialWorld :: World
initialWorld = World { player = V2 10 10
                     , ticksLastFrame = 0
                     , playerControl = PlayerControl SDL.Released SDL.Released
                     }

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Test SDL" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      worldRef <- newIORef initialWorld
      appLoop r worldRef

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
          SDL.KeycodeA -> \c -> c { moveLeft = motion }
          SDL.KeycodeD -> \c -> c { moveRight = motion }
          _ -> id
  _ -> id
  

updateWorld :: [SDL.Event] -> World -> Word32 -> World
updateWorld events world t =
  let deltaTime = fromIntegral (t - ticksLastFrame world) / 1000
      deltaPos = deltaTime * 20
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
      x = x_left + x_right
   in V2 x 0

drawWorld :: MonadIO m => SDL.Renderer -> World -> m ()
drawWorld renderer world = do
  let position = round <$> player world
  SDL.rendererDrawColor renderer SDL.$= V4 255 255 255 255
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
