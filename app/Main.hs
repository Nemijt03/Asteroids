module Main where

import Imports
import Asteroids
import State

main :: IO ()
main = do
    state <- standardState 
    -- display (InWindow "window" (1280, 720) (0,0)) black stateToPicture
    playIO (InWindow "Asteroids" (1280, 720) (0, 0)) -- Or FullScreen
              black            -- Background color
              10               -- Frames per second
              state            -- Initial state
              stateToPicture   -- View function
              input            -- Event function
              step  