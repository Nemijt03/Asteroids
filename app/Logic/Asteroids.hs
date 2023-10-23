module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Player (playerStateToPicture)
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


removeDeadEnemies :: State -> State
removeDeadEnemies = undefined

stateToPicture :: State -> IO Picture
stateToPicture state = 
    do
        --enemies <- enemiesToPicture (enemies state)
        --projectiles <- projectilesToPicture (projectiles state)
        --animations <- animationsToPicture (animations state)
        player <- playerStateToPicture (playerState state)
        --score <- scoreToPicture (score state)

        return (Pictures [
            --enemies,
            --projectiles,
            --animations,
            player--,
            --score
            ])

-- | Handle one iteration of the game
step :: Float -> State -> IO State
step secs state = return (stepGameState state)
            where s = secs --loose definition

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
inputKey (EventKey key Down _ _) s = handleAction (search key (inputs s)) s
inputKey _ s = s


handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotatePlayer (degToRad (-90))
                    | ua == TurnRight = rotatePlayer (degToRad 90)
                    | ua == Forward = accelerate 3
                    | ua == Backward = accelerate (-3)
                    | ua == Pause = case gameLoop s of
                        Running -> s {gameLoop = Paused}
                        Paused -> s {gameLoop = Running}
                    | otherwise = s
                    where 
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)