module Constants where

import Linear.V2

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
