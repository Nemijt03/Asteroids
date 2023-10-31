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
                playerReloadTime :: Float,
                playerBitmapData :: BitmapData
                }
                deriving (Show, Eq)

-- will multiply the first argument with the playerAcceleration vector
addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration f ps = ps { playerAcceleration = f PMath.* playerFacing ps}

-- will step playerState with time as an additional argument
stepPlayerState :: PlayerState -> Float -> PlayerState
stepPlayerState ps time =   ps {
                            playerPosition = wrap (mapPlus playerPosition playerSpeed ps),
                            playerSpeed = 0.9 PMath.* mapPlus playerSpeed playerAcceleration ps,
                            playerAcceleration = 0.8 PMath.* playerAcceleration ps,
                            playerReloadTime = playerReloadTime ps - 1
                        }

-- will convert PlayerState to picture
playerStateToPicture :: PlayerState -> IO Picture
playerStateToPicture ps = do
                    (w, h) <- getScreenSize
                    let bmp1 = Rotate 90 $ Bitmap $ playerBitmapData ps
                        (cx, cy) = (w `div` 2, h `div` 2)
                        pos = second (fromIntegral h -) (playerPosition ps)
                        (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)
                        rotation = radToDeg (argV (playerFacing ps))

                    return (Translate dx dy ( Rotate rotation bmp1))
                    -- return (Pictures [Color white (Text (show (playerPosition ps))), Translate 0 100 (Color white (Text (show dx')))])