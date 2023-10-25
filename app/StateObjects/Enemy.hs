module Enemy where

import Imports
import Assoc

data Enemy = MkAsteroid {
                asteroidPosition :: Point, 
                asteroidSpeed :: Vector,
                asteroidSize :: Size,
                asteroidHealth :: Int
            } 
            | 
            MkSaucer {
                saucerPosition :: Point,
                saucerSpeed :: Vector,
                saucerAcceleration :: Vector,
                saucerSize :: Size,
                saucerHealth :: Int,
                saucerReloadTime :: Float
            }
                deriving (Show, Eq)
data Size = Small | Medium | Large | ExtraLarge
                deriving (Show, Eq)

type SizeAssoc = Assoc Size Float

standardSize :: SizeAssoc
standardSize = [(Small, 10), (Medium, 25), (Large, 40), (ExtraLarge, 60)]

enemiesToPicture :: [Enemy] -> Picture
enemiesToPicture es = Pictures (enemiesToPictures es)

enemiesToPictures :: [Enemy] -> [Picture] -- These functions can be implemented in the Enemy.hs file
enemiesToPictures = undefined