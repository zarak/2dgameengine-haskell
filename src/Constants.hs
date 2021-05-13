module Constants where

import qualified SDL.Font
import SDL

projectileSize :: (Ord a, Num a) => V2 a
projectileSize = V2 10 10

rectangleSize :: (Ord a, Num a) => V2 a
rectangleSize = V2 16 100

framesPerSecond :: Float
framesPerSecond = 60

frameTargetTime :: Float
frameTargetTime = 1000 / framesPerSecond

ub :: (Num a, Ord a) => V2 a
ub = V2 640 480 - rectangleSize

lb :: (Num a, Ord a) => V2 a
lb = V2 0 0

opponentStartPosition :: V2 Float
opponentStartPosition = V2 (640 - distanceFromRightWall - playerSizeX) 100
 where
  (V2 playerSizeX _) = rectangleSize
  distanceFromRightWall = 6

playerStartPosition :: V2 Float
playerStartPosition = V2 6 10

green :: SDL.Font.Color
green = V4 0 255 0 0

white :: SDL.Font.Color
white = V4 255 255 255 0

localIP :: String
localIP = "192.168.100.23"
