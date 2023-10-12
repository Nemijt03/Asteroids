module Imports (module Graphics.Gloss, module Graphics.Gloss.Interface.IO.Game, module Graphics.Gloss.Data.Point, module Data.Maybe, mapPlus) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Data.Maybe
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

mapPlus :: (a -> Vector) -> (a -> Vector) -> a -> Vector
mapPlus f1 f2 s = f1 s PMath.+ f2 s