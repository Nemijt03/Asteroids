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


stepProjectile :: Projectile ->  Projectile
stepProjectile p = p {
                        projectilePosition = wrap (mapPlus projectilePosition projectileSpeed p),
                        projectileTimeAlive = projectileTimeAlive p - 1
                    }


--maybe it spawning on top will cause it to hit the point it spawned on

shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer s = Projectile {
                                    projectilePosition = playerPosition s PMath.+ 2 PMath.* playerFacing s,
                                    projectileSpeed = playerSpeed s PMath.+ (50 PMath.* playerFacing s),
                                    projectileTimeAlive = 20
                                }

shootFromSaucer :: Enemy -> Point -> Projectile
shootFromSaucer MkSaucer{saucerPosition = epos} ppos 
 = Projectile {
                projectilePosition  = epos PMath.+ ( 2 PMath.* (epos PMath.- ppos)),
                projectileSpeed     =  50 PMath.* (epos PMath.- ppos),
                projectileTimeAlive = 20
              } 
shootFromSaucer _ _ = undefined



projectilesToPicture :: [Projectile] -> IO Picture
projectilesToPicture lst = do
    bmp <- loadBMP "images\\ship32.bmp" -- projectile bmp ofc
    size <- getScreenSize
    return $ Pictures $ Prelude.map (\x -> projectileToPicture x bmp size) lst

projectileToPicture :: Projectile -> Picture -> (Int, Int) -> Picture
projectileToPicture p bmp size = Translate dx dy $ Scale sx sy $ Color white $ Text "."
    where 
        (w, h) = size
        (sx, sy) = (0.2, 0.2)
        -- bmp1 = Rotate 90 bmp
        (cx, cy) = (w `div` 2, h `div` 2)
        pos = second (fromIntegral h -) $ projectilePosition p
        (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)