{-# language NamedFieldPuns #-}
module State where

import Player
import Imports
import Projectile
import Collision
import Animation
import Enemy
import HandleInputs
import qualified Data.Set as S
import System.Random

data State = State {    -- All positions of the State will be defined in a 16:9 field, 
                        -- maybe 720p (1280x720) to create easy conversion on HD screens.
                        enemies :: [Enemy],
                        projectiles :: [Projectile],
                        animations :: [Animation],
                        playerState :: PlayerState,
                        score :: Int,
                        timePlayed :: Float,
                        gameLoop :: GameLoop,
                        randomG :: StdGen,
                        inputs :: Inputs,
                        downKeys :: S.Set Key
            }
            deriving (Show, Eq)

standardState :: State
standardState = State {
            enemies = [],
            projectiles = [], --Projectile (500, 360) (0,0) 10],
            animations = [mkDeathAnimation (600, 360)],
            playerState = PlayerState {
                playerPosition = (640, 360),
                playerFacing = normalizeV (1,0),
                playerSpeed = (0,0),
                playerAcceleration = (0,0),
                playerLives = 3,
                playerReloadTime = 0
            },
            score = 0,
            timePlayed = 0,
            gameLoop = Running,
            inputs = standardInputs,
            downKeys = S.empty,
            randomG = mkStdGen 10 --pseudo random. All executions of the program will result in the same
}



stepProjectiles :: State -> State
stepProjectiles s = s {projectiles = map stepProjectile (projectiles s)}

stepEnemies :: State -> State
stepEnemies s@State{playerState,enemies} = s{enemies = map ( `moveEnemy` playerPosition playerState) enemies}

stepAnimations :: State -> State
stepAnimations s = s {animations = map stepAnimation (animations s)}

stepEnemiesShoot :: State -> State
stepEnemiesShoot s@State{enemies,projectiles,playerState} = let (newE, newP) = foldr getProj ([],[]) enemies
                                                in s{enemies = newE, projectiles = projectiles ++ newP}
    where
        getProj e@MkSaucer{} (listE, listP) = (e{saucerReloadTime = 120}:listE,shootFromSaucer e (playerPosition playerState) : listP)
        getProj e (listE, listP)             = (e:listE, listP)

doCollision :: State -> State
doCollision s@State{enemies, projectiles, playerState} = let (newE, newPr, newPl) = naiveCollision enemies projectiles playerState
                                                     in s{enemies = newE, projectiles = newPr, playerState = newPl}


data GameLoop = Running | Paused | GameOver | GameQuitted | OptionsMenu
                deriving (Show, Eq, Enum)
