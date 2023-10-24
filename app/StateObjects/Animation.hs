module Animation where

import Imports
-- data Animation = DeathAnimation {
--                     frameTotal :: Int,
--                     onFrame :: Int,
--                     animationTime :: Float
--                 }
--                 | 
--                 SpawnAnimation {
                    
--                 }

data Animation = MkAnimation {
                        frameTotal :: Int,
                        onFrame :: Int,
                        timeTillNextFrame :: Float
                    }
    deriving (Show, Eq)
animationToPicture :: Animation -> Picture
animationToPicture = undefined

stepAnimation :: Animation -> Float -> Animation
stepAnimation = undefined