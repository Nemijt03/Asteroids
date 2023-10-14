module Player where

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
addAcceleration f ps = ps { playerAcceleration = f PMath.* playerFacing ps}

stepPlayerState :: PlayerState -> PlayerState
stepPlayerState ps =   ps {
                            playerPosition = wrap (mapPlus playerPosition playerSpeed ps),
                            playerSpeed = mapPlus playerSpeed playerAcceleration ps,
                            playerReloadTime = playerReloadTime ps - 1,
                            playerAcceleration = 0.1 PMath.* playerAcceleration ps 
                        }


-- the first argument is the size of the screen (as measured by the getScreenSize function)
playerStateToPicture :: (Int, Int) -> PlayerState -> Picture -> Picture
playerStateToPicture (w, h) ps bmp = Translate dx dy ( Rotate rotation bmp)
    where 
        (cx, cy) = (w `div` 2, h `div` 2)
        (dx, dy) = (dx', fromIntegral h - dy')
        (dx', dy') = playerPosition ps PMath.- (fromIntegral cx, fromIntegral cy)
        rotation = radToDeg (argV (playerFacing ps))