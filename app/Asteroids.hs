module Asteroids where

import Imports
import Player
import Projectile
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


data State = State {    -- All positions of the State will be defined in a 16:9 field, 
                        -- maybe 720p (1280x720) to create easy conversion on HD screens.
			enemies :: [Enemy],
			projectiles :: [Projectile],
			animations :: [Animation],
			playerState :: PlayerState,
			score :: Int,
			timePlayed :: Int,
			gameLoop :: GameLoop
            }
            deriving (Show, Eq)

standardState :: State
standardState = State {
            enemies = [],
			projectiles = [],
			animations = [],
			playerState = PlayerState {
                playerPosition = (640, 360),
                playerFacing = normalizeV (0,1),
                playerSpeed = (0,0),
                playerAcceleration = (0,0),
                playerLives = 3,
                playerReloadTime = 0
            },
			score = 0,
			timePlayed = 0,
			gameLoop = Running
}

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

stateToPicture :: State -> Picture -> (Int, Int) -> Picture
stateToPicture state playerBmp size = 
    Pictures 
        [
            -- enemiesToPicture (enemies state),
            -- projectilesToPicture (projectiles state),
            -- animationsToPicture (animations state),
            playerStateToPicture size (playerState state) playerBmp --,
            -- scoreToPicture (score state)
        ]

enemiesToPicture :: [Enemy] -> Picture
enemiesToPicture es = Pictures (enemiesToPictures es)

enemiesToPictures :: [Enemy] -> [Picture] -- These functions van be implemented in the Enemy.hs file
enemiesToPictures = undefined