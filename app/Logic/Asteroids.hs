{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE NamedFieldPuns #-}
module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Assoc
import Enemy
import Projectile
import System.Exit
import Collision
import qualified Data.Set as S
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Animation
import SpawnEnemies
import Renderable
import ButtonLogic
import LeaderBoardLogic
import Graphics.UI.GLUT (Size(Size))

-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


-- | Handle one iteration of the game
step :: Float -> State -> IO State
step time state = case gameLoop state of
    GameOver -> do
        if name state /= ""
            then do
                recordScore state
                return $ stepGameState time state{name = ""}
            else return $ stepGameState time state
    GameQuit -> exitSuccess
    _ -> return $ stepGameState time state

-- pure step function.
stepGameState :: Float -> State -> State
stepGameState time s =
    case gameLoop s of
                    Running -> stateFunctions (s {
                            playerState = stepPlayerState (playerState s) time,
                            timePlayed = timePlayed s + 1,
                            score = stepUpdateScore s
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

stepUpdateScore :: State -> Int
stepUpdateScore State{projectiles = pr, enemies = en, score = sc} =
    let playerPr = filter isFromPlayer pr
        in calculateScore playerPr en sc

calculateScore :: [Projectile] -> [Enemy] -> Int -> Int
calculateScore ps es sc = sc + foldr getScoreProjectile 0 ps
    where
        getScoreProjectile :: Projectile -> Int -> Int
        getScoreProjectile pr sc = foldr (seeIfHitEnemy pr) sc es

        --100 for the hit alone, 1000 for destruction asteroid, 2500 for destruction saucer
        seeIfHitEnemy :: Projectile -> Enemy -> Int -> Int
        seeIfHitEnemy pr en sc  | not (isDead en) && isCollision pr en
                                    = pointForHit + pointForDestruction
                                | otherwise         = sc
                                    where
                                        pointForHit = 100
                                        pointForDestruction =
                                            if isDead (snd $ collide pr en)
                                                then case en of
                                                    MkAsteroid{} -> asteroidDestruction
                                                    MkSaucer{}   -> saucerDestruction
                                                else 0
                                        asteroidDestruction = 1000
                                        saucerDestruction = 2500

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
    case gameLoop s of
        Running -> case e of
                EventMotion _ -> handleMouseMove $ inputKey e s
                _ -> rtrn

        _ -> case e of
                EventKey key Up _ _ -> case key of
                    MouseButton b -> case b of
                        LeftButton -> mouseClick $ inputKey e s
                        _ -> rtrn
                    _ -> rtrn
                _ -> rtrn

-- if a key is down, add to downKeys, but only when in the list of Useractions.
--pause is special, as it should only work on the frame it is pressed
inputKey :: Event -> State -> State
inputKey (EventKey key Down _ _) s =
    case gameLoop s of
        RecordScore -> case key of
                        (Char c)                  -> s{name = name s ++ [c]}
                        (SpecialKey KeyBackspace) -> s{name = init (name s)}
                        (SpecialKey KeyEnter)     -> s{gameLoop = GameOver}
                        _                         -> s
        _           -> case searched of
                        Nothing       -> s
                        (Just Pause)  -> handleAction Pause s
                        (Just action) -> if S.member action uaList
                                            then insertKey
                                            else s
                    where
                        insertKey = s{downKeys = S.insert key (downKeys s)}
                        searched = search key $ inputs s
                        uaList = case gameLoop s of
                            Running -> runningUserActions
                            _ -> pausedUserActions

-- remove key from downKeys
inputKey (EventKey key Up _ _) s = s {downKeys = S.delete key (downKeys s)}
inputKey (EventMotion pos) s = s {mousePosition = pos}
inputKey _ s = s

-- turn a Useraction into a change in the state
handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotatePlayer (-rotation)
                    | ua == TurnRight = rotatePlayer rotation
                    | ua == Forward = accelerate acceleration
                    | ua == Backward = accelerate (-acceleration)
                    | ua == Shoot = shootFromPlayer s
                    | ua == Pause = case gameLoop s of
                        Paused   -> s {gameLoop = Running}
                        GameOver -> s
                        _        -> s {gameLoop = Paused}
                    | ua == TriggerQuitGame = s {gameLoop = GameQuit}
                    | otherwise = s
                    where
                        rotation = degToRad 10
                        acceleration = 0.7
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)

handleMouseMove :: State -> IO State
handleMouseMove s = do 
    Size w h <- get windowSize
    let (cx, cy) = (w `div` 2, h `div` 2)
        relativePos = second (fromIntegral cy -) $ mousePosition s
        
        facing = normalizeV vec
        vec = (relativePos PMath.+ (fromIntegral cx, 0)) PMath.- playerPosition (playerState s)
    
    return s {
                        playerState = (playerState s) {playerFacing = facing},
                        options = (options s) {mouseInput = not $ mouseInput (options s)}
                    }

recordScore :: State -> IO ()
recordScore s@State{name} = toLeaderBoard (clampList 3 ' ' name, score s)

clampList :: Int -> a -> [a] -> [a]
clampList i e list | length list < i = list ++ replicate (i - length list) e
                   | otherwise       = take i list