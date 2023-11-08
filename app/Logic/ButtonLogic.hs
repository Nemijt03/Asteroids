{-# LANGUAGE TupleSections #-}
module ButtonLogic where

import State

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

-- event handler of clicking while paused
mouseClick :: State -> IO State
mouseClick s = do
    a <- case gameLoop s of
        Leaderboard -> mkButtonsNoActions $ mkLeaderboardButtons [("sef", 53),("fea",52),("as",10),("je",2)]
        _ -> pausingButtonsWithActions

    let mousePos = mousePosition s
        filteredList = filter (isInside mousePos) a
    if null filteredList
        then return s
        else snd (head filteredList) s

allButtonsWithActions :: IO [(Button, State -> IO State)]
allButtonsWithActions = do
    pausingButtons <- pausingButtonsWithActions
    leaderboardButtons <- mkButtonsNoActions $ mkLeaderboardButtons [("sef", 53),("fea",52),("as",10),("je",2)]
    return $ pausingButtons ++ leaderboardButtons

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
                        saveGame,
                        loadGame,
                        \s -> return $ s {gameLoop = Leaderboard}
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


mkButtonsNoActions :: [Button] -> IO [(Button, State -> IO State)]
mkButtonsNoActions btns = return $ map ( , return) btns

saveGame :: State -> IO State
saveGame = undefined

loadGame :: State -> IO State
loadGame = undefined


-- button data type for creating buttons in the UI
data Button = MkButton (Float, Float) (Float, Float) Color String | MkPicButton (Float, Float) Color Picture

-- will check if mousepos is inside the button
isInside :: Point -> (Button, a) -> Bool
isInside mousePos (MkButton xy wh _ _ , _) = pointInBox mousePos (xy PMath.+ (0.5 PMath.* wh)) (xy PMath.- (0.5 PMath.* wh))
isInside mousePos (MkPicButton xy _ _ , _) = pointInBox mousePos (xy PMath.+ (50, 50)) (xy PMath.- (50, 50))

-- -- button conversion to picture
-- buttonToPicture :: Button -> IO Picture
-- buttonToPicture (MkButton (x, y) (w, h) c s) = do
--                 width <- stringWidth Roman s
--                 let offset = fromIntegral $ negate $ width `div` 4
                
--                 return $ Translate x y $ Pictures [
--                         Color c $ rectangleWire w h,
--                         Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
--                     ]
-- -- Picture button is automatically square with size 100x100
-- buttonToPicture (MkPicButton (x, y) c pic) = return $ Translate x y $ Pictures [
--                                         Color c $ rectangleWire 100 100,
--                                         pic
--                                     ]

-- -- function to output one Picture for all buttons inputted
-- buttonsToPicture :: [Button] -> IO Picture
-- buttonsToPicture [] = return Blank
-- buttonsToPicture (x:xs) = do
--     button <- buttonToPicture x
--     rest <- buttonsToPicture xs
--     return $ Pictures [ button, rest]