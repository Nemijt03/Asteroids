module Main where

import Imports

main :: IO ()
main = do
    x <- loadBMP "images\\ship.bmp"
    display FullScreen black x