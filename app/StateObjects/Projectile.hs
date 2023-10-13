module Projectile where

import Imports
import ScreenLogic
import Enemy
import Player
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


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


shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer s = Projectile {
                                    projectilePosition = playerPosition s PMath.+ 2 PMath.* playerFacing s,
                                    projectileSpeed = 5 PMath.* playerFacing s,
                                    projectileTimeAlive = 10
                                }

shootFromSaucer :: Enemy -> Projectile
shootFromSaucer = undefined