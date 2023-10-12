module Projectile where

import Imports
import ScreenLogic

data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int
                }
                deriving (Show, Eq)

updateProjectileState :: Projectile -> Projectile
updateProjectileState p = p {
    projectilePosition = wrap (mapPlus projectilePosition projectileSpeed p),
    projectileTimeAlive = projectileTimeAlive p - 1
}