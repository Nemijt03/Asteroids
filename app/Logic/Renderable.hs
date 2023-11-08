{-# language NamedFieldPuns #-}
module Renderable where

import Imports
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Assoc
import Projectile
import Player
import Animation
import Enemy
import State
import ButtonLogic

import Graphics.UI.GLUT (Size(Size))
import Graphics.UI.GLUT.Fonts



stateToPicture :: State -> IO Picture
stateToPicture state =
    do
        enemiesPic <- translatedRender $ enemies state
        projectilesPic <- translatedRender $ projectiles state
        animationsPic <- translatedRender $ animations state
        player <- translatedRender $ playerState state

        pauseButtons <- pausingButtonsWithActions
        pauseButtonsPic <- translatedRender $ map fst pauseButtons

        leaderboardValues <- getLeaderBoard
        leaderboardButtonsPic <- translatedRender $ mkLeaderboardButtons leaderboardValues
        --score <- scoreToPicture (score state)
        -- let gameLoopShow = Color (makeColorI 255 255 255 0) $ Text $ show $ gameLoop state

        let gameLoopPictures = case gameLoop state of
                                Running -> []
                                Leaderboard -> [leaderboardButtonsPic]
                                _ -> [pauseButtonsPic]
        let testPictures = [
                            --Test:
                            -- Color white $ Text $ show $ toList $ downKeys state,
                        ]
        let statePictures = [
                            enemiesPic,
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
    render e = case e of 
            MkAsteroid{asteroidSize} -> Color yellow $ Circle (enemySize asteroidSize)
            MkSaucer{saucerSize} -> Color red $ Circle (enemySize saucerSize)
        where
            enemySize size = unsafeSearch size standardSize

    getPosition MkAsteroid{asteroidPosition} = asteroidPosition
    getPosition MkSaucer{saucerPosition} = saucerPosition

instance Renderable Button where
    render _ = Blank -- must define, but will not be used
    getPosition _ = (0,0) -- must define, but will not be used

    translatedRender b = do
        case b of

            (MkButton (x, y) (w, h) c s) -> do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                
                return $ Translate x y $ Pictures [
                        Color c $ rectangleWire w h,
                        Translate offset (-20) $ Scale 0.5 0.5 $ Color white $ Text s
                    ]

-- Picture button is automatically square with size 100x100
            (MkPicButton (x, y) c pic) -> return $ Translate x y $ Pictures [
                                        Color c $ rectangleWire 100 100,
                                        pic
                                    ]


instance (Renderable a) => Renderable [a] where
  render _ = Blank -- must define, but will not be used
  getPosition _ = (0,0) -- must define, but will not be used

  translatedRender lst = do 
    pics <- mapM translatedRender lst
    return $ Pictures pics