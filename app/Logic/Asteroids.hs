{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Assoc
import Graphics.UI.GLUT.Fonts
import System.Exit
import Projectile
import Collision
import qualified Data.Set as S
import Animation 
import SpawnEnemies
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath



button :: String -> Color -> IO Picture
button s c = do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                return $ Pictures [
                    Color c $ Line [(-300,-50), (300,-50), (300,50), (-300,50), (-300,-50)],
                    Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
                    ]

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
        --score <- scoreToPicture (score state)
        continueButton <- button "Continue (esc)" $ greyN 0.4
        optionsButton <- button "Options (o)" $ greyN 0.4
        quitButton <- button "Quit Game (0)" $ greyN 0.4
        -- let gameLoopShow = Color (makeColorI 255 255 255 0) $ Text $ show $ gameLoop state

        let pauseButtons = case gameLoop state of
                                Running -> []
                                _ ->        [    
                                                Translate 0 200 continueButton,
                                                optionsButton,
                                                Translate 0 (-200) quitButton
                                            ]
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
            pauseButtons ++ 
            testPictures)

-- | Handle one iteration of the game
step :: Float -> State -> IO State
step time state = do 
    print $ animations state
    case gameLoop state of
        GameQuitted -> exitSuccess
        _ -> return $ stepGameState time state

stepGameState :: Float -> State -> State
stepGameState time s = 
    case gameLoop s of
                    Running -> stateFunctions (s {
                            playerState = stepPlayerState (playerState s) time
                            -- stepScore (score state)
                            -- stepTimePlayed
                            -- stepGameLoop
                        })
                    _ -> stepDownKeys (downKeys s) s

    where stateFunctions = stepDownKeys (downKeys s) . 
                           spawnEnemy .
                           removeDeadObjects .
                           stepEnemiesShoot .
                           stepEnemies .
                           stepProjectiles . 
                           doCollision . 
                           stepAnimations

removeDeadObjects :: State -> State
removeDeadObjects s = let newA = mapMaybe maybeGetDeathAnimation (enemies s) in
                        s{enemies = removeDead (enemies s),
                        projectiles = removeDead (projectiles s),
                        animations = removeAnimations (animations s) ++ newA} 


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

inputKey :: Event -> State -> State
inputKey (EventKey key Down _ _) s | isJust searched && not (S.member (fromJust searched) uaList) = s  
                                   | otherwise = s {downKeys = S.insert key (downKeys s)}
                                   where 
                                    searched = search key $ inputs s
                                    uaList = if gameLoop s == Paused then pausedUserActions else runningUserActions
inputKey (EventKey key Up _ _) s = s {downKeys = S.delete key (downKeys s)}
inputKey _ s = s


handleAction :: UserAction -> State -> State
handleAction ua s   | ua == TurnLeft = rotatePlayer (degToRad (-90))
                    | ua == TurnRight = rotatePlayer (degToRad 90)
                    | ua == Forward = accelerate 3
                    | ua == Backward = accelerate (-3)
                    | ua == Shoot = s {projectiles = shootFromPlayer (playerState s) : projectiles s}
                    | ua == Pause = case gameLoop s of
                        Running -> s {gameLoop = Paused}
                        Paused -> s {gameLoop = Running}
                        _ -> s --gameLoop can only be running or Paused. This is unnecessary
                    | ua == QuitGame = s {gameLoop = GameQuitted}
                    | ua == Options = s {gameLoop = OptionsMenu}
                    | otherwise = s
                    where
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)