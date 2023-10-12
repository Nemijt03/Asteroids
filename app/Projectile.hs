module Projectile where

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int
                }
                deriving (Show, Eq)
