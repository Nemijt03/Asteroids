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

-- prototype
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
        Translate dx dy $ Scale sx sy $ pictureFrames a !! onFrame a


stepAnimation :: Animation -> Maybe Animation
stepAnimation a | checkTime && onFrame a + 1 == length (pictureFrames a) = Nothing
                | checkTime = Just $ a {timeFrameActive = 0, onFrame = onFrame a + 1}
                | otherwise = Just $ a {timeFrameActive = timeFrameActive a + 1}
                where 
                    checkTime = timeFrameActive a >= timePerFrame a

bmpDataToPieces :: BitmapData -> [Picture]
bmpDataToPieces bmpData = [
                            BitmapSection tl bmpData,
                            BitmapSection tr bmpData,
                            BitmapSection bl bmpData,
                            BitmapSection br bmpData
                        ]
                        where 
                            (w, h) = bitmapSize bmpData
                            tl = Rectangle (0,0) (div w 2, div h 2)
                            tr = Rectangle (div w 2, 0) (div w 2, div h 2)
                            bl = Rectangle (0, div h 2) (div w 2, div h 2)
                            br = Rectangle (div w 2, div h 2) (div w 2, div h 2)