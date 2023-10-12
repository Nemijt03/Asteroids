module Player where

import Projectile
import Imports 
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

data PlayerState = PlayerState {
                playerPosition :: Point,
                playerFacing :: Vector, -- Normalised vector
                playerSpeed :: Vector,
                playerAcceleration :: Vector,
                playerLives :: Int,
                playerReloadTime :: Int -- will be able to shoot when at 0
                }
                deriving (Show, Eq)

addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration = undefined

updatePlayerState :: PlayerState -> PlayerState
updatePlayerState s = s { 
    playerPosition = mapPlus playerPosition playerSpeed s,
    playerSpeed = mapPlus playerSpeed playerAcceleration s,
    playerReloadTime = playerReloadTime s - 1 
    }

updateProjectileState :: Projectile -> Projectile
updateProjectileState p = p {
    projectilePosition = mapPlus projectilePosition projectileSpeed p,
    projectileTimeAlive = projectileTimeAlive p - 1
}

mapPlus :: (a -> Vector) -> (a -> Vector) -> a -> Vector
mapPlus f1 f2 s = f1 s PMath.+ f2 s

shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer s = Projectile {
                                    projectilePosition = playerPosition s PMath.+ 2 PMath.* playerFacing s,
                                    projectileSpeed = 5 PMath.* playerFacing s,
                                    projectileTimeAlive = 10
                                }

playerStateToPicture :: PlayerState -> Picture
playerStateToPicture ps = playerPosition 