module Main where

import Imports
import Asteroids

main :: IO ()
main = do
    bmp <- loadBMP "images\\ship.bmp"
    size <- getScreenSize
    display (InWindow "window" (1280, 720) (0,0)) black (stateToPicture standardState x y)
    playIO (InWindow "Counter" (400, 400) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              standardState    -- Initial state
              (stateToPicture bmp size)   -- View function
              input            -- Event function
              step  