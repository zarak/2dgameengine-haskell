module State
  ( World(..)
  , PlayerControl(..)
  , initialWorld
  ) where

import SDL
import Data.Word (Word32)
import Constants

data World = World
  { player :: V2 Float
  , opponent :: V2 Float
  , ticksLastFrame :: Word32
  , playerControl :: PlayerControl
  , opponentControl :: PlayerControl
  , projectilePosition :: V2 Float
  , projectileDirection :: V2 Float
  , playerScore :: Integer
  , opponentScore :: Integer
  }
  deriving (Show)

data PlayerControl = PlayerControl
  { moveLeft :: SDL.InputMotion
  , moveRight :: SDL.InputMotion
  , moveUp :: SDL.InputMotion
  , moveDown :: SDL.InputMotion
  }
  deriving (Show)

initialWorld :: World
initialWorld =
  World
    { player = playerStartPosition
    , opponent = opponentStartPosition
    , ticksLastFrame = 0
    , playerControl = PlayerControl SDL.Released SDL.Released SDL.Released SDL.Released
    , opponentControl = PlayerControl SDL.Released SDL.Released SDL.Released SDL.Released
    , projectilePosition = V2 320 240 -- TODO: Randomize y position
    , projectileDirection = V2 (-1) 1 -- TODO: Randomize Direction
    , playerScore = 0
    , opponentScore = 0
    }

