module Main where

import Imports
import Asteroids
import Renderable
import State
import ScreenLogic

main :: IO ()
main = do
    state <- standardState
    -- display (InWindow "window" (1280, 720) (0,0)) black stateToPicture
    playIO FullScreen -- (InWindow "Asteroids" (1280, 720) (0, 0))
              black            -- Background color
              fps               -- Frames per second
              state            -- Initial state
              stateToPicture   -- View function
              input            -- Event function
              step