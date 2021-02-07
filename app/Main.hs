{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Common                 as C
import qualified SDL
import qualified SDL.Raw.Timer          as SDLTimer

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef
import           Foreign.C.Types        (CInt)
import           Linear
import Data.Word (Word32)

data World = World
  { player :: V2 CInt
  , ticksLastFrame :: Word32
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

      doRender :: World -> IO ()
      doRender = render renderer

  SDL.delay $ floor timeToWait'
  t' <- SDLTimer.getTicks

  let world' = updateWorld events world t'

  print world'
  doRender world'
  writeIORef worldRef world'
  unless qPressed (appLoop renderer worldRef)

updateWorld :: [SDL.Event] -> World -> Word32 -> World
updateWorld _ world t =
  let deltaTime = fromIntegral (t - ticksLastFrame world) / 1000
      deltaPos = V2 (floor $ deltaTime * 100) (floor $ deltaTime * 100)
  in
  World { player = player world + deltaPos
        , ticksLastFrame = t
        }

drawWorld :: MonadIO m => SDL.Renderer -> World -> m ()
drawWorld renderer world = do
  let position = player world
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
