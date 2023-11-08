{-# LANGUAGE DeriveGeneric #-}
module Player where

import ScreenLogic
import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import GHC.Generics

data PlayerState = PlayerState {
                playerPosition :: Point,
                playerFacing :: Vector, -- Normalised vector
                playerSpeed :: Vector,
                playerAcceleration :: Vector,
                playerLives :: Int,
                playerReloadTime :: Float,
                playerBitmapData :: BitmapData
                }
                deriving (Show, Eq,Generic)

-- will multiply the first argument with the playerAcceleration vector
addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration f ps = ps { playerAcceleration = f PMath.* playerFacing ps}

-- will step playerState with time as an additional argument
stepPlayerState :: PlayerState -> Float -> PlayerState
stepPlayerState ps _ =   ps {
                            playerPosition = wrap (mapPlus playerPosition playerSpeed ps),
                            playerSpeed = 0.9 PMath.* mapPlus playerSpeed playerAcceleration ps,
                            playerAcceleration = 0.8 PMath.* playerAcceleration ps,
                            playerReloadTime = playerReloadTime ps - 1
                        }