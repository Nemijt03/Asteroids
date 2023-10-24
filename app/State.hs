module State where

import Player
import Imports
import Projectile
import Animation
import Enemy
import HandleInputs

data State = State {    -- All positions of the State will be defined in a 16:9 field, 
                        -- maybe 720p (1280x720) to create easy conversion on HD screens.
			enemies :: [Enemy],
			projectiles :: [Projectile],
			animations :: [Animation],
			playerState :: PlayerState,
			score :: Int,
			timePlayed :: Float,
			gameLoop :: GameLoop,
            inputs :: Inputs
            }
            deriving (Show, Eq)

standardState :: State
standardState = State {
            enemies = [],
			projectiles = [],
			animations = [],
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
            inputs = standardInputs
}

data GameLoop = Running | Paused
                deriving (Show, Eq)
