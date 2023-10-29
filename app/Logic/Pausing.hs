module Pausing where

import Imports
import Graphics.UI.GLUT.Fonts

data ButtonSize = FullWidth | HalfWidth | Square Int
    deriving (Show, Eq)

button :: ButtonSize -> String -> Color -> IO Picture
button size s c = do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                    wire = case size of
                        FullWidth -> rectangleWire 600 100
                        HalfWidth -> rectangleWire 275 100
                        Square i -> rectangleWire (25 * fromIntegral i) (25 * fromIntegral i)
                return $ Pictures [
                    Color c wire,
                    Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
                    ]
iconButton :: Picture -> Color -> Picture
iconButton pic c = Pictures [
                                        Color c $ rectangleWire 100 100,
                                        pic
                                    ]

pauseButtonsIO :: IO [Picture]
pauseButtonsIO = do
    settingsPic <- loadBMP "images\\settings.bmp"
    continueButton <- button FullWidth "Continue (esc)" $ greyN 0.4
    quitButton <- button FullWidth "Quit Game (0)" $ greyN 0.4
    saveButton <- button HalfWidth "Save (s)" $ greyN 0.4
    loadButton <- button HalfWidth "Load (l)" $ greyN 0.4
    let icon = iconButton settingsPic $ greyN 0.4

    return [    
                Translate 400 300 icon,
                Translate 0 250 continueButton,
                Translate (-162) 100 saveButton,
                Translate 162 100 loadButton,
                Translate 0 (-50) quitButton
            ]