module ScreenLogic where

import Imports

-- the logic game screen size (as opposed to the world screen size)
screen :: (Float, Float)
screen = (1280, 720)

-- wrap the Point which is out of bounds of the screen to the other side of the screen
wrap :: Point -> Point
wrap (x, y) = (wrap' (fst screen) x, wrap' (snd screen) y)

-- helper to wrap one axis
wrap' :: Float -> Float -> Float
wrap' size pos | pos >= 0 && pos < size = pos
               | pos < 0 = size + pos
               | otherwise = pos - size