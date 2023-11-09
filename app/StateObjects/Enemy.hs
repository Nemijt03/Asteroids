{-# language NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module Enemy where

import Imports
import Assoc
import ScreenLogic
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import GHC.Generics
import Data.Aeson

instance ToJSON EnemySize
instance FromJSON EnemySize
instance FromJSON Enemy
instance ToJSON Enemy

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
                deriving (Show, Eq, Generic)

getEnemySize :: Enemy -> EnemySize
getEnemySize MkAsteroid{asteroidSize} = asteroidSize
getEnemySize MkSaucer{saucerSize} = saucerSize

data EnemySize = Small | Medium | Large | ExtraLarge
                deriving (Show, Eq, Ord, Generic)


moveEnemy :: Enemy -> Point -> Enemy --requires the playerPositon to be able to change acceleration
moveEnemy m@MkAsteroid{asteroidPosition = pos, asteroidSpeed = speed } _ = m{asteroidPosition = wrap $ pos PMath.+ speed}
moveEnemy s@MkSaucer{saucerAcceleration = acc, saucerPosition = pos, saucerReloadTime = rel, saucerSpeed = sp} playerPos =
        let updatedAcc = changeAcceleration acc pos playerPos
            updatedSp  = checkSpeed $ sp PMath.+ updatedAcc
        in s{saucerAcceleration = updatedAcc, 
             saucerPosition = wrap $ pos PMath.+ updatedSp, 
             saucerSpeed = updatedSp, 
             saucerReloadTime = rel - 1}
    where
        checkSpeed vec | magV vec > 8 = mulSV 8 $ normalizeV vec
                       | otherwise    = vec


---Quite simpel: Gets the vector to the player, normalizes it, then slightly rotates it so it does not head directly to the player. 
--maybe change it later so it depends on which side of the player and the distance, to deteremine what it will do.
changeAcceleration  :: Vector -> Point -> Point -> Vector
changeAcceleration acc saucerPos playerPos = let toPlayer = rotateV (degToRad 20) $ normalizeV $ playerPos PMath.- saucerPos
                                             in mulSV 0.3 $ acc PMath.+ toPlayer



type SizeAssoc = Assoc EnemySize Float

standardSize :: SizeAssoc
standardSize = [(Small, 10), (Medium, 25), (Large, 40), (ExtraLarge, 60)]