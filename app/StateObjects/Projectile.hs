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


stepProjectile :: Projectile -> Maybe Projectile
stepProjectile p | projectileTimeAlive p <= 0 = Nothing
                 | otherwise = Just $ p {
                        projectilePosition = wrap (mapPlus projectilePosition projectileSpeed p),
                        projectileTimeAlive = projectileTimeAlive p - 1
                    }


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