{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Common as C
import Data.IORef
import Runner (appLoop, initialWorld)
import qualified SDL.Font

main :: IO ()
main = C.withSDL $
  C.withSDLImage $ do
    C.setHintQuality
    C.withWindow "Test SDL" (640, 480) $ \w ->
      C.withRenderer w $ \r -> do
        SDL.Font.initialize
        font <- SDL.Font.load "fonts/OpenSans-Regular.ttf" 400
        worldRef <- newIORef initialWorld
        appLoop font r worldRef
