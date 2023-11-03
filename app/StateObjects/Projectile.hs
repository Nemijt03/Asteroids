module Projectile where

import Imports
import ScreenLogic
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int
                }
                deriving (Show, Eq)

-- will step projectile when timer is not finished
stepProjectile :: Projectile -> Maybe Projectile
stepProjectile p | projectileTimeAlive p <= 0 = Nothing
                 | otherwise = Just $ p {
                        projectilePosition = wrap (mapPlus projectilePosition projectileSpeed p),
                        projectileTimeAlive = projectileTimeAlive p - 1
                    }