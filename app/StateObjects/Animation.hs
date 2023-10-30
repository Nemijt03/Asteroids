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
mkDeathAnimation :: Point -> IO Animation
mkDeathAnimation position = do
    bmp1 <- loadBMP "images/explosion/1.bmp"
    bmp2 <- loadBMP "images/explosion/2.bmp"
    bmp3 <- loadBMP "images/explosion/3.bmp"
    bmp4 <- loadBMP "images/explosion/4.bmp"
    bmp5 <- loadBMP "images/explosion/5.bmp"
    bmp6 <- loadBMP "images/explosion/6.bmp"
    return $ MkAnimation {
                        animationPosition = position,
                        onFrame = 0,
                        timeFrameActive = 0,
                        timePerFrame = 10,
                        pictureFrames = [
                            Scale 1.25 1.25 bmp1,
                            Scale 1.25 1.25 bmp2,
                            Scale 1.25 1.25 bmp3,
                            Scale 1.25 1.25 bmp4,
                            Scale 1.25 1.25 bmp5,
                            Scale 1.25 1.25 bmp6
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