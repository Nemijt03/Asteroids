module ScreenLogic where

-- import Imports
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Data.Maybe

screen :: (Float, Float)
screen = (1280, 720)

wrap :: Vector -> Vector
wrap (x, y) | isJust (wrapx x) && isJust (wrapy y) = (fromJust (wrapx x), fromJust (wrapy y))
            | isJust (wrapx x) = (fromJust (wrapx x), y) 
            | isJust (wrapy y) = (x, fromJust (wrapy y))
            | otherwise = (x, y)

wrapx :: Float -> Maybe Float
wrapx x | x >= 0 && x < fst screen = Nothing
        | x < 0 = Just (fst screen + x)
        | otherwise = Just (x - fst screen)

wrapy :: Float -> Maybe Float
wrapy y | y >= 0 && y < snd screen = Nothing
        | y < 0 = Just (snd screen + y)
        | otherwise = Just (y - snd screen)