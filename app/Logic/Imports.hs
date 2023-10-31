module Imports (module Graphics.Gloss, 
                module Graphics.Gloss.Interface.IO.Game, 
                module Graphics.Gloss.Data.Point, 
                module Data.Maybe, 
                module Graphics.Gloss.Data.Vector,
                module Graphics.Gloss.Interface.Environment,
                module Graphics.Gloss.Geometry.Angle,
                module Data.Bifunctor,
                module Codec.BMP,
                mapPlus
                ) where

-- very great idea of the TA's, to implement imports and export all modules rightaway
-- also added a mapPlus just for simplicity in different functions

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point
import Data.Maybe
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss.Geometry.Angle
import Data.Bifunctor
import Codec.BMP
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

mapPlus :: (a -> Point) -> (a -> Point) -> a -> Point
mapPlus f1 f2 s = f1 s PMath.+ f2 s