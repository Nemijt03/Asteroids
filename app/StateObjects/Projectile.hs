{-# LANGUAGE DeriveGeneric #-}
module Projectile where

import Imports
import ScreenLogic
import Enemy
import Player
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import GHC.Generics
import qualified Data.Aeson as Ae

instance Ae.FromJSON Projectile
instance Ae.ToJSON Projectile

data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int,
                projectileImmuneTime  :: Int
                }
                deriving (Generic, Show, Eq)


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
                                    projectileSpeed = 30 PMath.* playerFacing s,
                                    projectileTimeAlive = 20,
                                    projectileImmuneTime = 3
                                    }

shootFromSaucer :: Enemy -> Point -> Projectile
shootFromSaucer MkSaucer{saucerPosition = epos} ppos 
 = Projectile {
                projectilePosition  = epos ,
                projectileSpeed     = mulSV 30 $ normalizeV (ppos PMath.- epos),
                projectileTimeAlive = 30,
                projectileImmuneTime = 3
              } 
shootFromSaucer _ _ = undefined



projectilesToPicture :: [Projectile] -> IO Picture
projectilesToPicture lst = do
    bmp <- loadBMP "images\\ship32.bmp" -- projectile bmp ofc
    size <- getScreenSize
    return $ Pictures $ Prelude.map (\x -> projectileToPicture x bmp size) lst

projectileToPicture :: Projectile -> Picture -> (Int, Int) -> Picture
projectileToPicture p _ size = Translate dx dy $ Scale sx sy $ Color white $ Text "."
    where 
        (w, h) = size
        (sx, sy) = (0.2, 0.2)
        -- bmp1 = Rotate 90 bmp
        (cx, cy) = (w `div` 2, h `div` 2)
        pos = second (fromIntegral h -) $ projectilePosition p
        (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)