{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Assoc
import System.Exit
import Collision
import qualified Data.Set as S
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Animation 
import SpawnEnemies
import Renderable
import ButtonLogic
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


-- | Handle one iteration of the game
step :: Float -> State -> IO State
step time state = do
    case gameLoop state of
        GameQuit -> exitSuccess
        _ -> return $ stepGameState time state

-- pure step function.
stepGameState :: Float -> State -> State
stepGameState time s =
    case gameLoop s of
                    Running -> stateFunctions (s {
                            playerState = stepPlayerState (playerState s) time,
                            timePlayed = timePlayed s + 1
                            -- stepGameLoop
                        })
                    _ -> stepDownKeys (downKeys s) s

    where stateFunctions = stepDownKeys (downKeys s)  
                           . spawnEnemy 
                           . stepEnemiesShoot 
                           . stepEnemies 
                           . stepProjectiles 
                           . doCollision
                           . checkPlayerDeath
                           . stepAnimations
                           . removeDeadObjects
                           . mkExplosions

mkExplosions :: State -> State
mkExplosions s = s {animations = animations s ++ newA}
    where 
        newA = map (makeEx . getPosition) (filter isDead (enemies s))
        makeEx = mkExplosion (explosion (loadedPictures s))

removeDeadObjects :: State -> State
removeDeadObjects s = s{enemies = removeDead (enemies s),
                        projectiles = removeDead (projectiles s),
                        animations = removeAnimations (animations s) } 

-- step through the set of keys which are being pressed at the time
stepDownKeys :: S.Set Key -> State -> State
stepDownKeys set s       = case S.toList set of
                                [] -> s
                                (key:keys) -> (if isJust searched
                                            then stepDownKeys (S.fromList keys) newState
                                            else stepDownKeys (S.fromList keys) s)
                                    where
                                        newState = handleAction (fromJust searched) s
                                        searched = search key standardInputs



-- | Handle user input
input :: Event -> State -> IO State
input e s = do
    let rtrn = return $ inputKey e s
    -- call mouseClick if LeftButton is clicked
    if gameLoop s /= Running
        then case e of
            EventKey key Up _ _ -> case key of
                MouseButton b -> case b of
                    LeftButton -> mouseClick $ inputKey e s
                    _ -> rtrn
                _ -> rtrn
            _ -> rtrn
        else rtrn

-- if a key is down, add to downKeys, but only when in the list of Useractions
inputKey :: Event -> State -> State
inputKey (EventKey key Down _ _) s | isJust searched && not (S.member (fromJust searched) uaList) = s
                                   | otherwise = s {downKeys = S.insert key (downKeys s)}
                                   where
                                    searched = search key $ inputs s
                                    uaList = case gameLoop s of
                                        Running -> runningUserActions
                                        _ -> pausedUserActions
                                        
-- remove key from downKeys
inputKey (EventKey key Up _ _) s = s {downKeys = S.delete key (downKeys s)}
inputKey (EventMotion pos) s | gameLoop s == Running = handleMouseMove $ s {mousePosition = pos} 
                             | otherwise = s {mousePosition = pos}
inputKey _ s = s

-- turn a Useraction into a change in the state
handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotatePlayer (degToRad (-10))
                    | ua == TurnRight = rotatePlayer (degToRad 10)
                    | ua == Forward = accelerate 0.7
                    | ua == Backward = accelerate (-0.7)
                    | ua == Shoot = shootFromPlayer s
                    | ua == Pause = case gameLoop s of
                        Paused -> s {gameLoop = Running}
                        _ -> s {gameLoop = Paused}
                    | ua == TriggerQuitGame = s {gameLoop = GameQuit}
                    | ua == TriggerOptions = s {gameLoop = OptionsMenu}
                    | otherwise = s
                    where
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)

handleMouseMove :: State -> State
handleMouseMove s = s {
                        playerState = (playerState s) {playerFacing = facing},
                        options = (options s) {mouseInput = not $ mouseInput (options s)}
                    }
                where
                    facing = normalizeV vec
                    vec = ((x, 360 - y) PMath.+ (640, 0)) PMath.- playerPosition (playerState s)
                    (x, y) = mousePosition s