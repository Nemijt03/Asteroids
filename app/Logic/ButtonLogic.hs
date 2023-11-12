{-# LANGUAGE TupleSections #-}
module ButtonLogic where

import State
import Player
import SavingAndLoading
import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

-- event handler of clicking while paused
mouseClick :: State -> IO State
mouseClick s = do
    a <- case gameLoop s of
        Leaderboard -> mkButtonsNoActions $ mkLeaderboardButtons [("sef", 53),("fea",52),("as",10),("je",2)]
        GameOver -> gameOverButtonsWithActions (score s)
        Saving ->   savingButtonsWithActions      
        Loading -> loadingButtonsWithActions
        Paused -> pausingButtonsWithActions
        _      -> mkButtonsNoActions [MkButton (0, 250) (600, 100) (greyN 0.4) ""]
    let mousePos = mousePosition s
        filteredList = filter (isInside mousePos) a
    if null filteredList
        then return s
        else snd (head filteredList) s


-- pausing buttons with their actions for mouseClick
pausingButtonsWithActions :: IO [(Button, State -> IO State)]
pausingButtonsWithActions = do
    settingsPic <- loadBMP "images\\settings.bmp"
    leaderboardPic <- loadBMP "images\\leaderboard.bmp"
    return $ zip    [ -- Buttons
                        MkPicButton (450, 300) (greyN 0.4) settingsPic,
                        MkButton (0, 250) (600, 100) (greyN 0.4) "Continue (esc)",
                        MkButton (0, -50) (600, 100) (greyN 0.4) "Quit Game (0)",
                        MkButton (-162, 100) (275, 100) (greyN 0.4) "Save (s)",
                        MkButton (162, 100) (275, 100) (greyN 0.4) "Load (l)",
                        MkPicButton (450, 150) (greyN 0.4) $ Scale 2 2 leaderboardPic
                    ]
                    [ -- Actions
                        \s -> return $ s {gameLoop = OptionsMenu},
                        \s -> return $ s {gameLoop = Running},
                        \s -> return $ s {gameLoop = GameQuitted},
                        \s -> return $ s {gameLoop = Saving},
                        \s -> return $ s {gameLoop = Loading},
                        \s -> return $ s {gameLoop = Leaderboard}
                    ]


getPlayerNameButtons :: String -> Int -> IO [(Button, State -> IO State)]
getPlayerNameButtons name score =
     mkButtonsNoActions $ [
                            MkButton (0, 250) (600, 100) (greyN 0.4) "Input your name in 3 letters:",
                            MkButton (0, 100) (600, 100) (greyN 0.4) ("Name: " ++ name),
                            MkButton (0, -50) (600, 100) (greyN 0.4) ("Score:" ++ show score)
                          ]

gameOverButtonsWithActions :: Int -> IO [(Button, State -> IO State)]
gameOverButtonsWithActions score = do
    return $ zip    [ -- Buttons
                        MkButton (0, 250) (600, 100) (greyN 0.4) "Restart",
                        MkButton (0, 100) (600, 100) (greyN 0.4) "Your Score was:",
                        MkButton (0, -50) (600, 100) (greyN 0.4) (show score),
                        MkButton (0, -200) (600, 100) (greyN 0.4) "Quit Game (0)"
                    ]
                    [ -- Actions
                        \s -> standardState,
                        \s -> return s,
                        \s -> return s,
                        \s -> return $ s {gameLoop = GameQuitted}
                    ]

mkLeaderboardButtons :: [(String, Int)] -> [Button]
mkLeaderboardButtons xs = f xs 0
    where
        f :: [(String, Int)] -> Int -> [Button]
        f [] _ = []
        f ((name, s):ss) i = 
            MkButton (x, 300 + fromIntegral (i `mod` 5) * (-100)) (350, 100) (greyN 0.4) (name ++ ": " ++ show s) : f ss (i + 1)
                where 
                    x = case i `div` 5 of
                            0 -> -350
                            1 -> 0
                            2 -> 350
                            _ -> 1500


savingButtonsWithActions :: IO [(Button, State -> IO State)]
savingButtonsWithActions = do
    fileResults <- mapM (\int -> checkExists (getFilePathToSave int)) [1 .. 5] --seeing what files already exists
    let availabilityStrings = zipWith (getSlotAvailability) [1 .. 5] fileResults
    return $ zip (zipWith createSaveButton [1 .. 5] availabilityStrings) --buttons themselves
                 (map actionPutSave [1 .. 5]) --actions with the buttons
    where
        createSaveButton int string = MkButton (0,250 - 100 * int) (600,80) (greyN 0.4) string 
        getSlotAvailability int False = "<Slot " ++ show int ++ " Available>" 
        getSlotAvailability int True  = "save " ++ show int
        actionPutSave int = \s -> do
            putStateToFile int s{gameLoop = Paused}
            return s{gameLoop = Paused}


loadingButtonsWithActions :: IO [(Button, State -> IO State)]
loadingButtonsWithActions = do
     fileResults <- mapM (\int -> checkExists (getFilePathToSave int)) [1 .. 5] --seeing what files already exists
     let availabilityStrings = zipWith getSlotAvailability [1 .. 5] fileResults
     return $ zip (zipWith createLoadButton [1 .. 5] availabilityStrings) --buttons themselves
                  (zipWith actionGetSave    [1 .. 5] fileResults) --actions with the buttons
     where 
        createLoadButton int string = MkButton (0,250 - 100 * int) (600,80) (greyN 0.4) string 
        getSlotAvailability int False = "<Save to this slot first>" 
        getSlotAvailability int True  = "save " ++ show int
        actionGetSave int False = \s -> return s
        actionGetSave int True = \s -> do
            newS <- getStateFromFile int
            case newS of
                Nothing       -> return s 
                Just newState -> return newState

mkButtonsNoActions :: [Button] -> IO [(Button, State -> IO State)]
mkButtonsNoActions btns = return $ map ( , return) btns


-- button data type for creating buttons in the UI
data Button = MkButton (Float, Float) (Float, Float) Color String | MkPicButton (Float, Float) Color Picture

-- will check if mousepos is inside the button
isInside :: Point -> (Button, a) -> Bool
isInside mousePos (MkButton xy wh _ _ , _) = pointInBox mousePos (xy PMath.+ (0.5 PMath.* wh)) (xy PMath.- (0.5 PMath.* wh))
isInside mousePos (MkPicButton xy _ _ , _) = pointInBox mousePos (xy PMath.+ (50, 50)) (xy PMath.- (50, 50))