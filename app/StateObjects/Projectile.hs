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

-- converts list of projectiles to picture using projectile to picture
projectilesToPicture :: [Projectile] -> IO Picture
projectilesToPicture lst = do
    bmp <- loadBMP "images\\ship32.bmp" -- projectile bmp
    size <- getScreenSize
    return $ Pictures $ Prelude.map (\x -> projectileToPicture x bmp size) lst

-- converts projectile to picture using bitmap and screen size (although bitmap is not implemented yet)
projectileToPicture :: Projectile -> Picture -> (Int, Int) -> Picture
projectileToPicture p bmp size = Translate dx dy $ Scale sx sy $ Color white $ Text "."
    where 
        (w, h) = size
        (sx, sy) = (0.2, 0.2)
        -- bmp1 = Rotate 90 bmp
        (cx, cy) = (w `div` 2, h `div` 2)
        pos = second (fromIntegral h -) $ projectilePosition p
        (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)