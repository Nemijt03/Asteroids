{-# language NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module State where

import Player
import Imports
import Projectile
import Collision
import Animation
import Enemy ( Enemy(MkSaucer, saucerReloadTime), moveEnemy )
import HandleInputs
import qualified Data.Set as S
import System.Random
import GHC.Generics
import qualified Data.Aeson as Ae

instance Ae.ToJSON Options
instance Ae.FromJSON Options

data State = State {    -- All positions of the State will be defined in a 16:9 field, 
                        -- maybe 720p (1280x720) to create easy conversion on HD screens.
                        enemies :: [Enemy],
                        projectiles :: [Projectile],
                        animations :: [Animation],
                        playerState :: PlayerState,
                        score :: Int,
                        timePlayed :: Float,
                        gameLoop :: GameLoop,
                        downKeys :: S.Set Key,
                        mousePosition :: (Float, Float),
                        options :: Options,
                        randomG :: StdGen,
                        inputs :: Inputs,
                        loadedPictures :: LoadedPictures,
                        name           :: String
            }
            deriving (Show, Eq)

data Options = MkOptions {
                        mouseInput :: Bool,
                        ietsAnders :: Bool
                    }
                        deriving (Generic, Show, Eq)

standardPictures :: IO LoadedPictures
standardPictures = do
    e1 <- loadBMP "images/explosion/1.bmp"
    e2 <- loadBMP "images/explosion/2.bmp"
    e3 <- loadBMP "images/explosion/3.bmp"
    e4 <- loadBMP "images/explosion/4.bmp"
    e5 <- loadBMP "images/explosion/5.bmp"
    e6 <- loadBMP "images/explosion/6.bmp"

    pBullet <- loadBMP "images/fire.bmp"
    eBullet <- loadBMP "images/fire1.bmp"
    sPic <- loadBMP "images/ship2.bmp"
    aPic <- loadBMP "images/asteroid.bmp"

    return $ MkPictures {
        playerBullet = pBullet,
        saucerBullet = eBullet,
        explosion = [e1, e2, e3, e4, e5, e6],
        saucerPicture = sPic,
        asteroidPicture = aPic
        }

data LoadedPictures = MkPictures {
                                    playerBullet :: Picture,
                                    saucerBullet :: Picture,
                                    explosion :: [Picture],
                                    saucerPicture :: Picture,
                                    asteroidPicture :: Picture

                                } 
                                deriving (Show, Eq)

standardState :: IO State
standardState = do
    rando <- initStdGen
    Right playerBMP <- readBMP "images\\ship32.bmp"
    standardPics <- standardPictures
    let playerBMPData = bitmapDataOfBMP playerBMP
    return $ State {
            enemies = [],
            projectiles = [], --Projectile (500, 360) (0,0) 30],
            animations = [],
            playerState = PlayerState {
                playerPosition = (640, 360),
                playerFacing = normalizeV (1,0),
                playerSpeed = (0,0),
                playerAcceleration = (0,0),
                playerLives = 3,
                playerReloadTime = 0,
                playerBitmapData = playerBMPData
            },
            score = 0,
            timePlayed = 0,
            gameLoop = Running,
            inputs = standardInputs,
            downKeys = S.empty,
            mousePosition = (0, 0),
            options = standardOptions,
            randomG = rando,
            loadedPictures = standardPics,
            name = ""
}

getPredictableRandom :: StdGen 
getPredictableRandom = mkStdGen 10 --this will always give the same result 

standardOptions :: Options
standardOptions = MkOptions {mouseInput = False, ietsAnders = False}


-- will step projectiles
stepProjectiles :: State -> State
stepProjectiles s = s {projectiles = map stepProjectile (projectiles s)}

stepEnemies :: State -> State
stepEnemies s@State{playerState,enemies} = s{enemies = map ( `moveEnemy` playerPosition playerState) enemies}

-- will step animations 
stepAnimations :: State -> State
stepAnimations s = s {animations = map stepAnimation (animations s)}

stepEnemiesShoot :: State -> State
stepEnemiesShoot s@State{enemies,projectiles,playerState} =
    let (newE, newP) = foldr getProj ([],[]) enemies
        in s{enemies = newE, projectiles = projectiles ++ newP}
    where
        getProj e@MkSaucer{saucerReloadTime} (listE, listP) | saucerReloadTime < 1 =
             (e{saucerReloadTime = 60}:listE,shootFromSaucer e (playerPosition playerState) : listP)
                                                            | otherwise            =
             (e:listE, listP)
        getProj e (listE, listP)             = (e:listE, listP)

doCollision :: State -> State
doCollision s@State{enemies, projectiles, playerState} =
    let (newE, newPr, newPl) = naiveCollision enemies projectiles playerState
        in s{enemies = newE, projectiles = newPr, playerState = newPl}

checkPlayerDeath :: State -> State
checkPlayerDeath s@State{playerState} | isDead playerState = s{gameLoop = RecordScore}
                                      | otherwise          = s
-- will be death of player and spawn in death animation
playerDies :: State -> State
playerDies s = s -- make bitmap go into pieves and explode it like that (doesn't this then need to be IO?)

-- shoots a projectile from the player in the direction it's facing
-- also adds the speed of the projectile to the speed of the player
-- so if the player is moving backwards, the projectile will go less fast
shootFromPlayer :: State -> State
shootFromPlayer s | playerReloadTime (playerState s) > 0 = s
                  | otherwise = s {projectiles = projectileFromPlayer (playerState s) : projectiles s,
                                   playerState = (playerState s) {playerReloadTime = 5} }




data GameLoop = Running | Paused | GameOver | GameQuit | Leaderboard | RecordScore | Saving | Loading
                deriving (Show, Eq, Enum, Generic)

instance Ae.FromJSON GameLoop
instance Ae.ToJSON GameLoop