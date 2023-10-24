module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Assoc
import Data.Set
import Data.Set (empty)
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
            --Test:
            -- Color white $ Text $ show $ toList $ downKeys state,
            
            --enemies,
            --projectiles,
            --animations,
            player--,
            --score
            ])

-- | Handle one iteration of the game
step :: Float -> State -> IO State
step time state = return (stepGameState state)

stepGameState :: State -> State
stepGameState s = stepDownKeys (downKeys s) (s {
                            -- stepEnemies (enemies state),
                            -- stepProjectiles (projectiles state),
                            -- stepAnimations (animations state),
                            playerState = stepPlayerState (playerState s) 0
                            -- stepScore (score state)
                            -- stepTimePlayed
                            -- stepGameLoop
                        })

stepDownKeys :: Set Key -> State -> State
stepDownKeys set s       = case toList set of
                                [] -> s
                                _ -> (if isJust searched 
                                            then stepDownKeys keys newState 
                                            else stepDownKeys keys s)
                                where
                                    newState = handleAction (fromJust (search key standardInputs)) s
                                    searched = search key standardInputs
                                    key = head $ toList set
                                    keys = fromList $ tail $ toList set

-- | Handle user input
input :: Event -> State -> IO State
input e s = return $ inputKey e s

inputKey :: Event -> State -> State
inputKey (EventKey key Down _ _) s = s {downKeys = insert key (downKeys s)}
inputKey (EventKey key Up _ _) s = s {downKeys = delete key (downKeys s)}
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