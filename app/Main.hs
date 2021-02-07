{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Common                 as C
import qualified SDL

import           Control.Monad.IO.Class (MonadIO)
import           Linear
import Control.Monad (unless)
import Foreign.C.Types (CInt)

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
      appLoop r

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents
  let qPressed = any eventIsQPress events
      doRender :: World -> IO ()
      doRender = render renderer

  doRender $ foldr updateWorld initialWorld events
  -- render renderer initialWorld

  unless qPressed (appLoop renderer)

updateWorld :: SDL.Event -> World -> World
updateWorld _ world = World { object = object world + 1 }
-- updateWorld _ = id

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

rectanglePosition :: V2 CInt
rectanglePosition = V2 10 10
