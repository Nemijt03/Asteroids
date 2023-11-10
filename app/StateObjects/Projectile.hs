module Projectile where

import Imports
import ScreenLogic
import Enemy
import Player
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int,
                projectileImmuneTime  :: Int,
                isFromPlayer :: Bool
                }
                deriving (Show, Eq)


stepProjectile :: Projectile ->  Projectile
stepProjectile p = p {
                        projectilePosition = wrap (projectilePosition p PMath.+ projectileSpeed p),
                        projectileTimeAlive = projectileTimeAlive p - 1,
                        projectileImmuneTime = projectileImmuneTime p - 1
                     }


--maybe it spawning on top will cause it to hit the point it spawned on

projectileFromPlayer :: PlayerState -> Projectile
projectileFromPlayer s = Projectile {
                                    projectilePosition = playerPosition s,
                                    projectileSpeed = 20 PMath.* playerFacing s,
                                    projectileTimeAlive = 30,
                                    projectileImmuneTime = 3,
                                    isFromPlayer = True
                                    }

shootFromSaucer :: Enemy -> Point -> Projectile
shootFromSaucer MkSaucer{saucerPosition = epos} ppos 
 = Projectile {
                projectilePosition  = epos ,
                projectileSpeed     = mulSV 10 $ normalizeV (ppos PMath.- epos),
                projectileTimeAlive = 30,
                projectileImmuneTime = 3,
                isFromPlayer = False
              } 
shootFromSaucer _ _ = undefined