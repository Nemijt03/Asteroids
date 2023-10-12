module Player where

import Projectile
import ScreenLogic
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
updatePlayerState s =   s {
                            playerPosition = wrap (mapPlus playerPosition playerSpeed s),
                            playerSpeed = mapPlus playerSpeed playerAcceleration s,
                            playerReloadTime = playerReloadTime s - 1  
                            -- Add functionality for gradually decreasing the acceleration
                        }


shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer s = Projectile {
                                    projectilePosition = playerPosition s PMath.+ 2 PMath.* playerFacing s,
                                    projectileSpeed = 5 PMath.* playerFacing s,
                                    projectileTimeAlive = 10
                                }

playerStateToPicture :: (Int, Int) -> PlayerState -> Picture -> Picture
playerStateToPicture (w, h) ps bmp = Translate dx dy (bmp)
    where 
        centre@(cx, cy) = (w `div` 2, h `div` 2)
        dx = 1
        dy = 1