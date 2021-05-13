module Physics
  ( clamp
  , updateProjectileDirection
  , playerCollision
  , opponentCollision
  , rightWallCollision
  , leftWallCollision
  ) where

import SDL
import State (World (projectileDirection, opponent), player, projectilePosition)
import Control.Applicative (liftA2)
import Constants

clamp :: (Num a, Ord a) => V2 a -> V2 a
clamp = liftA2 min ub . liftA2 max lb

updateProjectileDirection :: World -> V2 Float
updateProjectileDirection w
  | playerCollision (player w) (projectilePosition w) = V2 (abs dx) dy
  | opponentCollision opponentPosition (projectilePosition w) = V2 (- abs dx) dy
  | y > 480 - projectileSizeY || y < 0 = V2 dx (- dy)
  | otherwise = V2 dx dy
 where
  (V2 x y) = projectilePosition w
  (V2 dx dy) = projectileDirection w
  (V2 _ projectileSizeY) = projectileSize
  opponentPosition = opponent w

playerCollision :: (Ord a, Num a) => V2 a -> V2 a -> Bool
playerCollision (V2 x y) (V2 px py) =
  py > y && py < y + playerSizeY -- within paddle height
    && px < x + playerSizeX && px > x
 where
  (V2 playerSizeX playerSizeY) = rectangleSize

opponentCollision :: (Ord a, Num a) => V2 a -> V2 a -> Bool
opponentCollision (V2 x y) (V2 px py) =
  (py > y) && (py < y + playerSizeY) 
    && px' < x + playerSizeX && px' > x
 where
  px' = px + projectileSizeX
  (V2 playerSizeX playerSizeY) = rectangleSize
  (V2 projectileSizeX _) = projectileSize

rightWallCollision :: (Ord a, Num a) => V2 a -> Bool
rightWallCollision (V2 x y) = x > (640 - projectileSizeX) 
 where
  (V2 projectileSizeX _) = projectileSize

leftWallCollision :: (Ord a, Num a) => V2 a -> Bool
leftWallCollision (V2 x y) = x < 0
 where
  (V2 projectileSizeX _) = projectileSize
