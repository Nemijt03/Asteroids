{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Asteroids where

import Imports
import Player
import Pausing
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
import SavingAndLoading
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


-- | Handle one iteration of the game
step :: Float -> State -> IO State
step time state = do
    case gameLoop state of
        GameQuitted -> exitSuccess
        _ -> do 
            newA <- mapM (mkExplosion . getPosition) (filter isDead (enemies state))
            return $ stepGameState time state{animations = animations state ++ newA}

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
                           . stepAnimations 
                           . removeDeadObjects 

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
                                    uaList = if gameLoop s == Paused then pausedUserActions else runningUserActions
-- remove key from downKeys
inputKey (EventKey key Up _ _) s = s {downKeys = S.delete key (downKeys s)}
inputKey (EventMotion pos) s | gameLoop s == Running = handleMouseMove $ s {mousePosition = pos} 
                             | otherwise = s {mousePosition = pos}
inputKey _ s = s

-- turn a Useraction into a change in the state
handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotatePlayer (degToRad (-20))
                    | ua == TurnRight = rotatePlayer (degToRad 20)
                    | ua == Forward = accelerate 3
                    | ua == Backward = accelerate (-3)
                    | ua == Shoot = shootFromPlayer s
                    | ua == Pause = case gameLoop s of
                        Running -> s {gameLoop = Paused}
                        Paused -> s {gameLoop = Running}
                        Saving -> s{gameLoop = Paused}
                        Loading -> s {gameLoop = Paused}
                    | ua == TriggerQuitGame = s {gameLoop = GameQuitted}
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

-- event handler of clicking while paused
mouseClick :: State -> IO State
mouseClick s = 
    case (gameLoop s) of
        Saving ->  getAction savingButtonsWithActions      
        Paused ->  getAction buttonsWithActions
        Loading -> getAction loadingButtonsWithActions
    where
        getAction buttons = do
            a <- buttons      
            let mousePos = mousePosition s 
            let    result = filter (isInside mousePos) a
            if null result
                then return s
                else snd (head result) s