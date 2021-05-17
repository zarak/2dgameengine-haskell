module Renderer
  ( drawScore
  , drawCenterLine
  , drawProjectile
  , drawWorld
  ) where

import Control.Monad.IO.Class (MonadIO)
import qualified SDL.Font
import SDL.Font (Font)
import SDL
import qualified Data.Text as T
import qualified Common as C

import State
import Constants


drawScore :: MonadIO m => Font -> SDL.Renderer -> World -> m ()
drawScore font r w = do
  fontSurface <- SDL.Font.blended font white (T.pack $ show (playerScore w) <> "   " <> show (opponentScore w))
  scoreSprite <- toTexture fontSurface
  SDL.copyEx r scoreSprite Nothing (Just $ floor <$> C.mkRect 270 10 100 50) 0.0 Nothing (V2 False False)
  SDL.destroyTexture scoreSprite
  SDL.freeSurface fontSurface
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
