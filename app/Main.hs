{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Common                 as C
import qualified SDL

import           Control.Monad          (unless)
import           Control.Monad.IO.Class (MonadIO)
import           Data.IORef
import           Foreign.C.Types        (CInt)
import           Linear

newtype World = World
  { object :: V2 CInt
  }

initialWorld :: World
initialWorld = World { object = V2 10 10 }

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Test SDL" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      worldRef <- newIORef (World (V2 10 10))
      appLoop r worldRef

appLoop :: SDL.Renderer -> IORef World -> IO ()
appLoop renderer worldRef = do
  events <- SDL.pollEvents

  world <- readIORef worldRef

  let qPressed = any eventIsQPress events

      doRender :: World -> IO ()
      doRender = render renderer

      world' = updateWorld events world

  doRender world'

  writeIORef worldRef world'

  unless qPressed (appLoop renderer worldRef)

updateWorld :: [SDL.Event] -> World -> World
updateWorld _ world = World { object = object world + 1 }

drawWorld :: MonadIO m => SDL.Renderer -> World -> m ()
drawWorld renderer world = do
  let position = object world
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
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 0 255
  SDL.clear renderer

  drawWorld renderer world
  
  SDL.present renderer

rectangleSize :: V2 CInt
rectangleSize = V2 20 20
