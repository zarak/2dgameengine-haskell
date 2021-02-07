{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MyLib (someFunc)
import qualified SDL
import qualified Common as C

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Loops    (iterateUntilM)
import Data.Foldable (foldl')

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Test SDL" (640, 480) $ \w ->
    C.withRenderer w $ \r -> do

      putStrLn "Hello"
