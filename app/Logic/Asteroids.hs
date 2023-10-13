module Asteroids where

import Imports
import Player
import Projectile
import HandleInputs
import State
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

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

stateToPicture :: Picture -> (Int, Int) -> State -> Picture
stateToPicture playerBmp size state = 
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

enemiesToPictures :: [Enemy] -> [Picture] -- These functions can be implemented in the Enemy.hs file
enemiesToPictures = undefined


-- | Handle one iteration of the game
step :: Float -> State -> IO State
step secs state = stepGameState state

stepGameState :: State -> State
stepGameState s = s   {
                            -- stepEnemies (enemies state),
                            -- stepProjectiles (projectiles state),
                            -- stepAnimations (animations state),
                            playerState = stepPlayerState (playerState s)
                            -- stepScore (score state)
                            -- stepTimePlayed
                            -- stepGameLoop
                            -- stepInputs
                        }

-- | Handle user input
input :: Event -> State -> IO State
input e s = return (inputKey e s)

inputKey :: Event -> State -> State
inputKey (EventKey key _ _ _) s = handleAction (search key (inputs s)) s
inputKey _ s = s


handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotate (-1.5708)
                    | ua == TurnRight = rotate 1.5708
                    | ua == Forward = accelerate 10
                    | ua == Backward = accelerate -10
                    | ua == Pause = case gameLoop s of
                        Running -> s {gameLoop = Paused}
                        Paused -> s {gameLoop = Running}
                    where 
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotate rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)