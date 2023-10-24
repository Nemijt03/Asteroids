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
                playerReloadTime :: Float -- will be able to shoot when at 0
                }
                deriving (Show, Eq)

addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration f ps = ps { playerAcceleration = f PMath.* playerFacing ps}

stepPlayerState :: PlayerState -> Float -> PlayerState
stepPlayerState ps time =   ps {
                            playerPosition = wrap (mapPlus playerPosition playerSpeed ps),
                            playerSpeed = mapPlus playerSpeed playerAcceleration ps,
                            playerReloadTime = playerReloadTime ps - 1,
                            playerAcceleration = 0.02 PMath.* playerAcceleration ps
                        }


playerStateToPicture :: PlayerState -> IO Picture
playerStateToPicture ps = do
                    bmp <- loadBMP "images\\ship32.bmp"
                    (w, h) <- getScreenSize
                    let bmp1 = Rotate 90 bmp
                        (cx, cy) = (w `div` 2, h `div` 2)
                        pos = second (fromIntegral h -) (playerPosition ps)
                        (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)
                        rotation = radToDeg (argV (playerFacing ps))

                    return (Translate dx dy ( Rotate rotation bmp1))
                    -- return (Pictures [Color white (Text (show (playerPosition ps))), Translate 0 100 (Color white (Text (show dx')))])