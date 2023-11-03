module Enemy where

import Imports
import Assoc

data Enemy = MkAsteroid {
                asteroidPosition :: Point, 
                asteroidSpeed :: Vector,
                asteroidSize :: EnemySize,
                asteroidHealth :: Int
            } 
            | 
            MkSaucer {
                saucerPosition :: Point,
                saucerSpeed :: Vector,
                saucerAcceleration :: Vector,
                saucerSize :: EnemySize,
                saucerHealth :: Int,
                saucerReloadTime :: Float
            }
                deriving (Show, Eq)
data EnemySize = Small | Medium | Large | ExtraLarge
                deriving (Show, Eq)

type SizeAssoc = Assoc EnemySize Float

standardSize :: SizeAssoc
standardSize = [(Small, 10), (Medium, 25), (Large, 40), (ExtraLarge, 60)]