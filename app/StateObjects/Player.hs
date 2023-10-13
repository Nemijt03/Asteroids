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

stepPlayerState :: PlayerState -> PlayerState
stepPlayerState s =   s {
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

-- the first argument is the size of the screen (as measured by the getScreenSize function)
playerStateToPicture :: (Int, Int) -> PlayerState -> Picture -> Picture
playerStateToPicture (w, h) ps bmp = Rotate rotation (Translate dx dy (bmp))
    where 
        (cx, cy) = (w `div` 2, h `div` 2)
        (dx, dy) = playerPosition ps PMath.- (fromIntegral cx, fromIntegral cy)
        rotation = radToDeg (argV (playerFacing ps))