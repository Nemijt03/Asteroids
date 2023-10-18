module Enemy where

import Imports

data Enemy = Asteroid | Saucer 
                deriving (Show, Eq)
data Asteroid = MkAsteroid {
                asteroidPosition :: Point, 
                asteroidSpeed :: Vector,
                asteroidSize :: Size,
                asteroidHealth :: Int
                }
                deriving (Show, Eq)
data Saucer = MkSaucer {
                saucerPosition :: Point,
                saucerSpeed :: Vector,
                saucerAcceleration :: Vector,
                saucerSize :: Size,
                saucerHealth :: Int,
                saucerReloadTime :: Int
                }
                deriving (Show, Eq)
data Size = Small | Medium | Large | ExtraLarge
                deriving (Show, Eq)

type SizeAssoc = Assoc Size Int

standardSize :: SizeAssoc
standardSize = [(Small, 10), (Medium, 25), (Large, 40), (ExtraLarge, 60)]

enemiesToPicture :: [Enemy] -> Picture
enemiesToPicture es = Pictures (enemiesToPictures es)

enemiesToPictures :: [Enemy] -> [Picture] -- These functions can be implemented in the Enemy.hs file
enemiesToPictures = undefined