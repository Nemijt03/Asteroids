module Pausing where

import Imports
import Graphics.UI.GLUT.Fonts


data Button = MkButton (Float, Float) (Float, Float) Color String | MkPicButton (Float, Float) Color Picture

buttonToPicture :: Button -> IO Picture
buttonToPicture (MkButton (x, y) (w, h) c s) = do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                
                return $ Translate x y $ Pictures [
                        Color c $ rectangleWire w h,
                        Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
                    ]
buttonToPicture (MkPicButton (x, y) c pic) = return $ Translate x y $ Pictures [
                                        Color c $ rectangleWire 100 100,
                                        pic
                                    ]

pausingButtons :: IO [Button]
pausingButtons = do 
    settingsPic <- loadBMP "images\\settings.bmp"
    return [
                MkPicButton (400, 300) (greyN 0.4) settingsPic,
                MkButton (0, 250) (600, 100) (greyN 0.4) "Continue (esc)",
                MkButton (0, -50) (600, 100) (greyN 0.4) "Quit Game (0)",
                MkButton (-162, 100) (275, 100) (greyN 0.4) "Save (s)",
                MkButton (162, 100) (275, 100) (greyN 0.4) "Load (l)"
            ]

buttonsToPicture :: [Button] -> IO Picture
buttonsToPicture [] = return Blank
buttonsToPicture (x:xs) = do
    button <- buttonToPicture x
    rest <- buttonsToPicture xs
    return $ Pictures [ button, rest]