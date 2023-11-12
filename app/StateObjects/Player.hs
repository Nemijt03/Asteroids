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
stepPlayerState ps _ =   ps {
                            playerPosition = wrap (mapPlus playerPosition playerSpeed ps),
                            playerSpeed = decrementSpeed PMath.* mapPlus playerSpeed playerAcceleration ps,
                            playerAcceleration = decrementAcc PMath.* playerAcceleration ps,
                            playerReloadTime = playerReloadTime ps - 1
                        }
                        where
                            decrementSpeed = 0.9
                            decrementAcc = 0.8