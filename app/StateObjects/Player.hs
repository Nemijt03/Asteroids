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
playerStateToPicture :: PlayerState -> IO Picture
playerStateToPicture ps = do
                    bmp <- loadBMP "images\\ship32.bmp"
                    let bmp1 = Rotate 90 bmp
                    (w, h) <- getScreenSize
                    --let  = size
                    let (cx, cy) = (w `div` 2, h `div` 2)
                    let (dx', dy') = playerPosition ps PMath.- (fromIntegral cx, fromIntegral cy)
                    let (dx, dy) = (dx', {-fromIntegral h - -}dy')
                    let rotation = radToDeg (argV (playerFacing ps))

                    return (Translate dx dy ( Rotate rotation bmp1))