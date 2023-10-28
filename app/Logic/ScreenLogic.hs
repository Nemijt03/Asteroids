module ScreenLogic where

import Imports

screen :: (Float, Float)
screen = (1280, 720)

screenWidth :: Float
screenWidth = fst screen

screenHeight :: Float
screenHeight = snd screen

wrap :: Vector -> Vector
wrap (x, y) = (wrapx x, wrapy y)

wrapx :: Float -> Float
wrapx x | x >= 0 && x < fst screen = x
        | x < 0 = fst screen + x
        | otherwise = x - fst screen

wrapy :: Float -> Float
wrapy y | y >= 0 && y < snd screen = y
        | y < 0 = snd screen + y
        | otherwise = y - snd screen

wrapPoint :: Point -> Point
wrapPoint (x, y) = (wrapx x, wrapy y)