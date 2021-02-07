{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Common                 as C
import qualified MyLib                  (someFunc)
import qualified SDL

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Loops    (iterateUntilM)
import           Data.Foldable          (foldl')
import           Linear
import Control.Monad (unless)

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Test SDL" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      putStrLn "Hello"

      appLoop r

appLoop :: SDL.Renderer -> IO ()
appLoop renderer = do
  events <- SDL.pollEvents
  let eventIsQPress event =
        case SDL.eventPayload event of
          SDL.KeyboardEvent keyboardEvent ->
            SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed &&
            SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent) == SDL.KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  render renderer
  unless qPressed (appLoop renderer)


render :: (MonadIO m) => SDL.Renderer -> m ()
render renderer = do
  SDL.rendererDrawColor renderer SDL.$= V4 0 0 255 255
  SDL.clear renderer
  SDL.present renderer

