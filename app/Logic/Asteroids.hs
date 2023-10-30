{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Assoc
import System.Exit
import Projectile
import Pausing
import qualified Data.Set as S
import Animation (animationsToPicture)
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


removeDeadEnemies :: State -> State
removeDeadEnemies = undefined


stateToPicture :: State -> IO Picture
stateToPicture state =
    do
        --enemies <- enemiesToPicture (enemies state)
        projectilesPic <- projectilesToPicture (projectiles state)
        animationsPic <- animationsToPicture (animations state)
        -- let gameLoopShow = case gameLoop state of
            -- Running -> 
            -- Paused ->
            -- GameOver ->
        player <- playerStateToPicture (playerState state)
        btns <- pausingButtons
        pauseButtons <- buttonsToPicture btns
        --score <- scoreToPicture (score state)
        -- let gameLoopShow = Color (makeColorI 255 255 255 0) $ Text $ show $ gameLoop state

        let pausePictures = case gameLoop state of
                                Running -> []
                                _ -> [pauseButtons]
        let testPictures = [
                            --Test:
                            -- Color white $ Text $ show $ toList $ downKeys state,
                        ]
        let statePictures = [
                            --enemies,
                            projectilesPic,
                            animationsPic,
                            player--,
                            -- gameLoopShow --,
                            --score
                        ] 


        return (Pictures $ 
            statePictures ++
            pausePictures ++ 
            testPictures)

-- | Handle one iteration of the game
step :: Float -> State -> IO State
step time state = do 
    case gameLoop state of
        GameQuitted -> exitSuccess
        _ -> return $ stepGameState time state

stepGameState :: Float -> State -> State
stepGameState time s = 
    case gameLoop s of
                    Running -> stateFunctions (s {
                            -- stepEnemies (enemies state),
                            playerState = stepPlayerState (playerState s) time
                            -- stepScore (score state)
                            -- stepTimePlayed
                            -- stepGameLoop
                        })
                    _ -> stepDownKeys (downKeys s) s

    where stateFunctions = stepDownKeys (downKeys s) . stepProjectiles . stepAnimations

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
input e s = return $ inputKey e s

-- if a key is down, add to downKeys, but only when in the list of Useractions
inputKey :: Event -> State -> State
inputKey (EventKey key Down _ _) s | isJust searched && not (S.member (fromJust searched) uaList) = s  
                                   | otherwise = s {downKeys = S.insert key (downKeys s)}
                                   where 
                                    searched = search key $ inputs s
                                    uaList = if gameLoop s == Paused then pausedUserActions else runningUserActions
-- remove key from downKeys and call mouseClick if LeftButton is clicked
inputKey (EventKey key Up _ _) s = case key of
                                MouseButton b -> case b of
                                    LeftButton -> mouseClick newS
                                    _ -> newS
                                _ -> newS
                                where newS = s {downKeys = S.delete key (downKeys s)}
inputKey (EventMotion pos) s = s {mousePosition = pos}
inputKey _ s = s

handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotatePlayer (degToRad (-90))
                    | ua == TurnRight = rotatePlayer (degToRad 90)
                    | ua == Forward = accelerate 3
                    | ua == Backward = accelerate (-3)
                    | ua == Shoot = shootFromPlayer s
                    | ua == Pause = case gameLoop s of
                        Running -> s {gameLoop = Paused}
                        Paused -> s {gameLoop = Running}
                        _ -> s
                    | ua == QuitGame = s {gameLoop = GameQuitted}
                    | ua == Options = s {gameLoop = OptionsMenu}
                    | otherwise = s
                    where
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)