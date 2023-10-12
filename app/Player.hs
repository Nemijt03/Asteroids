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
    projectileTimeAlive = projectileTimeAlive - 1
}

mapPlus :: (PlayerState -> Vector) -> (PlayerState -> Vector) -> PlayerState -> Vector
mapPlus f1 f2 s = f1 s PMath.+ f2 s

instance Num Vector where
    (a1, a2) + (b1, b2) = (a1 + b1, a2 + b2)
    (a1, a2) - (b1, b2) = (a1 - b1, a2 - b2)
    (a1, a2) * (b1, b2) = (a1 * b1, a2 * b2)
    negate (a1, a2) = ((-1) * a1, (-1) * a2)


shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer s = Projectile {
                                    projectilePosition = playerPosition s + playerFacing s * 2,
                                    projectileSpeed = playerFacing s * 5,
                                    projectileTimeAlive = 10
                                }