module Renderable where

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Projectile
import Player
import Animation
import Enemy
import State

import Graphics.UI.GLUT (Size(Size))
import Pausing


stateToPicture :: State -> IO Picture
stateToPicture state =
    do
        --enemies <- enemiesToPicture (enemies state)
        projectilesPic <- translatedRender $ projectiles state
        animationsPic <- translatedRender $ animations state
        player <- translatedRender $ playerState state

        btns <- buttonsWithActions
        let btns1 = map fst btns
        pauseButtons <- buttonsToPicture btns1
        --score <- scoreToPicture (score state)
        -- let gameLoopShow = Color (makeColorI 255 255 255 0) $ Text $ show $ gameLoop state

        let gameLoopPictures = case gameLoop state of
                                Running -> []
                                _ -> [pauseButtons]
        let testPictures = [
                            --Test:
                            -- Color white $ Text $ show $ toList $ downKeys state,
                        ]
        let statePictures = [
                            --enemies,
                            projectilesPic,
                            animationsPic,
                            player--,
                            -- gameLoopShow --,
                            --score
                        ]


        return (Pictures $
            statePictures ++
            gameLoopPictures ++
            testPictures)



class Renderable a where
    render :: a -> Picture
    getPosition :: a -> (Float, Float)
    translatedRender :: a -> IO Picture
    translatedRender a = do
        Size w h <- get windowSize
        let (cx, cy) = (w `div` 2, h `div` 2)
            pos = second (fromIntegral h -) $ getPosition a
            (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)

        return $ Translate dx dy $ render a




instance Renderable Projectile where
    render _ = Color white $ Text "."

    getPosition = projectilePosition


instance Renderable PlayerState where
    render player = Rotate rotation bmp
        where
            bmp = Rotate 90 $ Bitmap $ playerBitmapData player
            rotation = radToDeg (argV (playerFacing player))

    getPosition = playerPosition

instance Renderable Animation where
    render animation = pictureFrames animation !! onFrame animation

    getPosition = animationPosition

instance Renderable Enemy where
    render _ = undefined
    getPosition = undefined


instance (Renderable a) => Renderable [a] where
  render _ = Blank -- must define, but will not be used
  getPosition _ = (0,0) -- must define, but will not be used
  translatedRender lst = do 
    pics <- mapM translatedRender lst
    return $ Pictures pics