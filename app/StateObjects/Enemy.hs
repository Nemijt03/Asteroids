module Enemy where

import Imports
import Assoc
import ScreenLogic
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

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
                deriving (Show, Eq, Ord)


moveEnemy :: Enemy -> Point -> Enemy --requires the playerPositon to be able to change acceleration
moveEnemy m@MkAsteroid{asteroidPosition = pos, asteroidSpeed = speed } _ = m{asteroidPosition = wrapPoint $ pos PMath.+ speed}
moveEnemy s@MkSaucer{saucerAcceleration = acc, saucerPosition = pos, saucerReloadTime = rel, saucerSpeed = sp} playerPos =
        let updatedAcc = changeAcceleration acc pos playerPos
            updatedSp  = sp PMath.+ updatedAcc
        in s{saucerAcceleration = updatedAcc, 
             saucerPosition = wrapPoint $ pos PMath.+ sp, 
             saucerSpeed = updatedSp, 
             saucerReloadTime = rel - 1}
    


---Quite simpel: Gets the vector to the player, normalizes it, then slightly rotates it so it does not head directly to the player. 
--maybe change it later so it depends on which side of the player and the distance, to deteremine what it will do.
changeAcceleration  :: Vector -> Point -> Point -> Vector
changeAcceleration acc saucerPos playerPos = let toPlayer = rotateV (degToRad 10) $ normalizeV $ playerPos PMath.- saucerPos
                                             in acc PMath.+ toPlayer


type SizeAssoc = Assoc Size Float

standardSize :: SizeAssoc
standardSize = [(Small, 10), (Medium, 25), (Large, 40), (ExtraLarge, 60)]

enemiesToPicture :: [Enemy] -> Picture
enemiesToPicture es = Pictures (enemiesToPictures es)

enemiesToPictures :: [Enemy] -> [Picture] -- These functions can be implemented in the Enemy.hs file
enemiesToPictures = undefined