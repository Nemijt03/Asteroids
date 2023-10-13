module Asteroids where

import Imports
import Player
import HandleInputs
import State
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


removeDeadEnemies :: State -> State
removeDeadEnemies = undefined


stateToPicture :: Picture -> (Int, Int) -> State -> IO Picture
stateToPicture playerBmp size state = 
    return (Pictures 
        [
            -- enemiesToPicture (enemies state),
            -- projectilesToPicture (projectiles state),
            -- animationsToPicture (animations state),
            playerStateToPicture size (playerState state) playerBmp --,
            -- scoreToPicture (score state)
        ])


-- | Handle one iteration of the game
step :: Float -> State -> IO State
step secs state = return (stepGameState state)
            where s = secs

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
handleAction ua s   | ua == TurnLeft = rotatePlayer (-1.5708)
                    | ua == TurnRight = rotatePlayer 1.5708
                    | ua == Forward = accelerate 10
                    | ua == Backward = accelerate (-10)
                    | ua == Pause = case gameLoop s of
                        Running -> s {gameLoop = Paused}
                        Paused -> s {gameLoop = Running}
                    | otherwise = s
                    where 
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)