{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use :" #-}
module Asteroids where

import Imports
import Player
import HandleInputs
import State
import Assoc
import Graphics.UI.GLUT.Fonts
import Graphics.UI.GLUT (exit)
import System.Exit
-- import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


removeDeadEnemies :: State -> State
removeDeadEnemies = undefined


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
        --projectiles <- projectilesToPicture (projectiles state)
        --animations <- animationsToPicture (animations state)
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
                            --projectiles,
                            --animations,
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
    case gameLoop state of
        GameQuitted -> exitSuccess
        _ -> return $ stepGameState time state

stepGameState :: Float -> State -> State
stepGameState time s = case gameLoop s of
                    Running -> stepDownKeys (downKeys s) (s {
                            -- stepEnemies (enemies state),
                            -- stepProjectiles (projectiles state),
                            -- stepAnimations (animations state),
                            playerState = stepPlayerState (playerState s) time
                            -- stepScore (score state)
                            -- stepTimePlayed
                            -- stepGameLoop
                        })
                    _ -> stepDownKeys (downKeys s) s

stepDownKeys :: Set Key -> State -> State
stepDownKeys set s       = case toList set of
                                [] -> s
                                (key:keys) -> (if isJust $ searched key
                                            then stepDownKeys (fromList keys) $ newState key 
                                            else stepDownKeys (fromList keys) s)
                            where
                                newState key = handleAction (fromJust $ searched key) s
                                searched key = search key standardInputs

-- | Handle user input
input :: Event -> State -> IO State
input e s = return $ inputKey e s

inputKey :: Event -> State -> State
inputKey (EventKey key Down _ _) s | isJust searched && not (member (fromJust searched) uaList) = s  
                                   | otherwise = s {downKeys = insert key (downKeys s)}
                                   where 
                                    searched = search key $ inputs s
                                    uaList = if gameLoop s == Paused then pausedUserActions else runningUserActions
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
                        _ -> s
                    | ua == QuitGame = s {gameLoop = GameQuitted}
                    | ua == Options = s {gameLoop = OptionsMenu}
                    | otherwise = s
                    where
                        ps = playerState s
                        chngPs x = s {playerState = x}
                        rotatePlayer rotation = chngPs (ps {playerFacing = rotation `rotateV` playerFacing ps})
                        accelerate acceleration = chngPs (addAcceleration acceleration ps)