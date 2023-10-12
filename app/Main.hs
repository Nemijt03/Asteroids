module Main where

import Imports
import Asteroids

main :: IO ()
main = do
    x <- loadBMP "images\\ship.bmp"
    y <- getScreenSize
    display FullScreen black (stateToPicture standardState x y)