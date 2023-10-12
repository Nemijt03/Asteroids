module Asteroids where

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

main :: IO ()
main = return ()

data State = State {
			enemies :: [Enemy],
			projectiles :: [Projectile],
			animations :: [Animation],
			playerState :: PlayerState,
			score :: Int,
			timePlayed :: Int,
			gameLoop :: GameLoop
            }
            deriving (Show, Eq)

data GameLoop = Running | Paused
                deriving (Show, Eq)



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


shootFromSaucer :: Enemy -> Projectile
shootFromSaucer = undefined

removeDead :: State -> State
removeDead = undefined

data Animation = DeathAnimation | SpawnAnimation
                deriving (Show, Eq)
newtype DeathAnimation = MkDeathAnimation Int
                deriving (Show, Eq)
newtype SpawnAnimation = MkSpawnAnimation Int
                deriving (Show, Eq)
-- maybe dus functie voor animation :: Int -> Picture

stateToPicture :: State -> Picture
stateToPicture state = 
    Pictures 
        [
            playerStateToPicture (playerState state),
            enemiesToPicture
        ]

enemiesToPicture :: [Enemy] -> Picture
enemiesToPicture = Pictures enemiesToPictures

enemiesToPictures :: [Enemy] -> [Picture] -- These functions van be implemented in the Enemy.hs file
enemiesToPictures = undefined