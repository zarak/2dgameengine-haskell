{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Common                 as C
import GameState (appLoop, initialWorld)
import           Data.IORef

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Test SDL" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do
      worldRef <- newIORef initialWorld
      appLoop r worldRef
