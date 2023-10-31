module Pausing where

import Imports
import Graphics.UI.GLUT.Fonts
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


data Button = MkButton (Float, Float) (Float, Float) Color String | MkPicButton (Float, Float) Color Picture


isInside :: Point -> (Button, a) -> Bool
isInside mousePos (MkButton xy wh _ _ , _) = pointInBox mousePos (xy PMath.+ (0.5 PMath.* wh)) (xy PMath.- (0.5 PMath.* wh))
isInside mousePos (MkPicButton xy _ _ , _) = pointInBox mousePos (xy PMath.+ (50, 50)) (xy PMath.- (50, 50))

buttonToPicture :: Button -> IO Picture
buttonToPicture (MkButton (x, y) (w, h) c s) = do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                
                return $ Translate x y $ Pictures [
                        Color c $ rectangleWire w h,
                        Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
                    ]
buttonToPicture (MkPicButton (x, y) c pic) = return $ Translate x y $ Pictures [
                                        Color c $ rectangleWire 100 100,
                                        pic
                                    ]

buttonsToPicture :: [Button] -> IO Picture
buttonsToPicture [] = return Blank
buttonsToPicture (x:xs) = do
    button <- buttonToPicture x
    rest <- buttonsToPicture xs
    return $ Pictures [ button, rest]