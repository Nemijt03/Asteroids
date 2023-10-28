module Animation where

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

data Animation = MkAnimation {
                        animationPosition :: Point,
                        onFrame :: Int,
                        timeFrameActive :: Float,
                        timePerFrame :: Float,
                        pictureFrames :: [Picture]
                    }
                    deriving (Eq, Show)

mkDeathAnimation :: Point -> Animation
mkDeathAnimation position = MkAnimation {
                        animationPosition = position,
                        onFrame = 0,
                        timeFrameActive = 0,
                        timePerFrame = 20,
                        pictureFrames = [
                            Color white $ Circle 4,
                            Color white $ Circle 8,
                            Color white $ Circle 16,
                            Color white $ Circle 32,
                            Color white $ Circle 64,
                            Color white $ Circle 128
                        ]   
                    }

animationsToPicture :: [Animation] -> IO Picture
animationsToPicture as = do
    size <- getScreenSize
    return $ Pictures $ map (`animationToPicture` size) as

animationToPicture :: Animation -> (Int, Int) -> Picture
animationToPicture a (w, h) = do
    let (sx, sy) = (1, 1)
        (cx, cy) = (w `div` 2, h `div` 2)
        pos = second (fromIntegral h -) $ animationPosition a
        (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)
        in
        Translate dx dy $ Scale sx sy $ {-head $ -} pictureFrames a !! onFrame a


stepAnimation :: Animation -> Animation
stepAnimation a | checkTime =  a {timeFrameActive = 0, onFrame = onFrame a + 1}
                | otherwise =  a {timeFrameActive = timeFrameActive a + 1}
                where 
                    checkTime = timeFrameActive a >= timePerFrame a
-- | checkTime && onFrame a + 1 == length (pictureFrames a) = Nothing