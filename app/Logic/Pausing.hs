module Pausing where

import Imports
import Graphics.UI.GLUT.Fonts


data Button = MkButton (Float, Float) Color String | MkPicButton Color Picture

buttonToPicture :: Button -> IO Picture
buttonToPicture (MkButton (w, h) c s) = do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                
                return $ Pictures [
                        Color c $ rectangleWire w h,
                        Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
                    ]
buttonToPicture (MkPicButton c pic) = return $ Pictures [
                                        Color c $ rectangleWire 100 100,
                                        pic
                                    ]

pausingButtons :: IO [((Float, Float), Button)]
pausingButtons = do 
    settingsPic <- loadBMP "images\\settings.bmp"
    return [
                ((400, 300), MkPicButton (greyN 0.4) settingsPic),
                ((0, 250), MkButton (600, 100) (greyN 0.4) "Continue (esc)"),
                ((0, -50), MkButton (600, 100) (greyN 0.4) "Quit Game (0)"),
                ((-162, 100), MkButton (275, 100) (greyN 0.4) "Save (s)"),
                ((162, 100), MkButton (275, 100) (greyN 0.4) "Load (l)")
            ]

buttonsToPicture :: [((Float, Float), Button)] -> IO Picture
buttonsToPicture [] = return Blank
buttonsToPicture (x:xs) = do
    button <- translatedBTP x
    rest <- buttonsToPicture xs
    return $ Pictures [ button, rest]

translatedBTP :: ((Float, Float), Button) -> IO Picture
translatedBTP ((x, y), b) = do
    button <- buttonToPicture b
    return $ Translate x y button