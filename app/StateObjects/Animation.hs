module Animation where

import Imports
-- Animation data type
data Animation = MkAnimation {
                        animationPosition :: Point,
                        isRepeating :: Bool,
                        onFrame :: Int,
                        timeFrameActive :: Float,
                        timePerFrame :: Float,
                        pictureFrames :: [Picture]
                    }
                    deriving (Eq, Show)

-- creates explosion on specified Point with bitmaps loaded from file
mkExplosion :: [Picture] -> Point -> Animation
mkExplosion pics position = MkAnimation {
                        animationPosition = position,
                        isRepeating = False,
                        onFrame = 0,
                        timeFrameActive = 0,
                        timePerFrame = 10,
                        pictureFrames = map (Scale sF sF) pics
                    }
                    where sF = 1.5

stepAnimation :: Animation -> Animation
stepAnimation a | checkTime =  a {timeFrameActive = 0, onFrame = onFrame a + 1}
                | otherwise =  a {timeFrameActive = timeFrameActive a + 1}
                where 
                    checkTime = timeFrameActive a >= timePerFrame a

removeAnimations :: [Animation] -> [Animation]
removeAnimations = foldr func []
            where
                func x xs = if onFrame x + 1 == length (pictureFrames x) && checkTime x && not (isRepeating x)
                    then xs else x:xs
                checkTime a = timeFrameActive a >= timePerFrame a


-- will devide bmpdata into different sections for easy explosion (proof of concept for bitmap explosion)
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