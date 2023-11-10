{-# language NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
module State where

import Player
import Imports
import Projectile
import Collision
import Animation
import Enemy
import HandleInputs
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import qualified Data.Set as S
import Pausing
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
                        inputs :: Inputs
            }
            deriving (Show, Eq)

data Options = MkOptions {
                        mouseInput :: Bool,
                        ietsAnders :: Bool
                    }
                        deriving (Generic, Show, Eq)

standardState :: IO State
standardState = do
    Right playerBMP <- readBMP "images\\ship32.bmp"
    deathAnimation <- mkExplosion (600, 360)
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
            randomG = getPredictableRandom
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
stepEnemiesShoot s@State{enemies,projectiles,playerState} = let (newE, newP) = foldr getProj ([],[]) enemies
                                                in s{enemies = newE, projectiles = projectiles ++ newP}
    where
        getProj e@MkSaucer{saucerReloadTime} (listE, listP) | saucerReloadTime < 1 =
             (e{saucerReloadTime = 60}:listE,shootFromSaucer e (playerPosition playerState) : listP)
                                                            | otherwise            =
             (e:listE, listP)
        getProj e (listE, listP)             = (e:listE, listP)

doCollision :: State -> State
doCollision s@State{enemies, projectiles, playerState} = let (newE, newPr, newPl) = naiveCollision enemies projectiles playerState
                                                     in s{enemies = newE, projectiles = newPr, playerState = newPl}


-- will be death of player and spawn in death animation
playerDies :: State -> State
playerDies s = s -- make bitmap go into pieves and explode it like that

-- shoots a projectile from the player in the direction it's facing
-- also adds the speed of the projectile to the speed of the player
-- so if the player is moving backwards, the projectile will go less fast
shootFromPlayer :: State -> State
shootFromPlayer s | playerReloadTime (playerState s) > 0 = s
                  | otherwise = s {projectiles = projectileFromPlayer (playerState s) : projectiles s,
                                   playerState = (playerState s) {playerReloadTime = 5} }



-- pausing buttons with their actions for mouseClick
buttonsWithActions :: IO [(Button, State -> IO State)]
buttonsWithActions = do
    settingsPic <- loadBMP "images\\settings.bmp"
    leaderboardPic <- loadBMP "images\\leaderboard.bmp"
    return $ zip    [ -- Buttons
                        MkPicButton (450, 300) (greyN 0.4) settingsPic,
                        MkButton (0, 250) (600, 100) (greyN 0.4) "Continue (esc)",
                        MkButton (0, -50) (600, 100) (greyN 0.4) "Quit Game (0)",
                        MkButton (-162, 100) (275, 100) (greyN 0.4) "Save (s)",
                        MkButton (162, 100) (275, 100) (greyN 0.4) "Load (l)",
                        MkPicButton (450, 150) (greyN 0.4) $ Scale 2 2 leaderboardPic
                    ]
                    [ -- Actions
                        \s -> return $ s {gameLoop = OptionsMenu},
                        \s -> return $ s {gameLoop = Running},
                        \s -> return $ s {gameLoop = GameQuitted},
                        \s -> return $ s {gameLoop = Saving},
                        \s -> return $ s {gameLoop = Loading},
                        \s -> return $ s {gameLoop = Leaderboard}
                    ]




saveGame :: State -> IO State
saveGame = undefined

loadGame :: State -> IO State
loadGame = undefined

data GameLoop = Running | Paused | GameOver | GameQuitted | OptionsMenu | Leaderboard | Saving | Loading
                deriving (Show, Eq, Enum, Generic)

instance Ae.FromJSON GameLoop
instance Ae.ToJSON GameLoop
