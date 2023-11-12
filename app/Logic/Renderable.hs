{-# language NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}
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
import LeaderBoardLogic

stateToPicture :: State -> IO Picture
stateToPicture state =
    do
        enemiesPic <- enemies state `translatedRender` state
        projectilesPic <- projectiles state `translatedRender`  state
        animationsPic <- animations state `translatedRender` state
        player <- playerState state `translatedRender` state

        pauseButtons <- pausingButtonsWithActions
        pauseButtonsPic <- map fst pauseButtons `translatedRender` state

        leaderboardValues <- getLeaderBoard
        leaderboardButtonsPic <- mkLeaderboardButtons leaderboardValues `translatedRender` state

        gameOverButtons <- gameOverButtonsWithActions (score state)
        gameOverButtonsPic <- map fst gameOverButtons `translatedRender` state

        saveButtons <- savingButtonsWithActions
        saveButtonsPic <- map fst saveButtons `translatedRender` state

        loadButtons <- loadingButtonsWithActions
        loadButtonsPic <- map fst loadButtons `translatedRender` state

        nameButtons <- getPlayerNameButtons (name state) (score state)
        nameButtonsPic <- map fst nameButtons `translatedRender` state
        -- let gameLoopShow = Color (makeColorI 255 255 255 0) $ Text $ show $ gameLoop state

        let scorePic = Translate 300 300 $ Scale 0.4 0.4 $ Color white $ Text $ "Score: " ++ show (score state)
        let livesPic = Translate (-300) 300 $ Scale 0.4 0.4 $ Color white $ Text ("Lives: " ++ show (playerLives (playerState state)))

        let gameLoopPictures = case gameLoop state of
                                Paused ->  [pauseButtonsPic]
                                Saving ->  [saveButtonsPic]
                                Loading -> [loadButtonsPic]
                                Leaderboard -> [leaderboardButtonsPic]
                                GameOver -> [gameOverButtonsPic]
                                RecordScore -> [nameButtonsPic]
                                _ -> []
        let testPictures = [
                            --Test:
                            -- Color white $ Text $ show $ toList $ downKeys state,
                        ]
        let statePictures = [
                            enemiesPic,
                            projectilesPic,
                            animationsPic,
                            player,
                            livesPic,
                            scorePic
                        ]


        return (Pictures $
            statePictures ++
            gameLoopPictures ++
            testPictures)


class Renderable a where
    render :: a -> State -> Picture
    render _ _ = Blank
    renderIO :: a -> State -> IO Picture
    renderIO _ _ = return Blank
    getPosition :: a -> (Float, Float)
    translatedRender :: a -> State -> IO Picture
    translatedRender a s = do
        Size w h <- get windowSize
        picIO <- renderIO a s
        let (cx, cy) = (w `div` 2, h `div` 2)
            pos = second (fromIntegral h -) $ getPosition a
            (dx, dy) = pos PMath.- (fromIntegral cx, fromIntegral cy)
            pic = case picIO of
                Blank -> render a s
                _ -> picIO

        return $ Translate dx dy pic




instance Renderable Projectile where
    render projectile s =
        Rotate rotation $ Rotate standardRotation pic
            where 
                rotation = radToDeg $ argV $ normalizeV $ projectileSpeed projectile
                pic | isFromPlayer projectile = playerBullet $ loadedPictures s
                    | otherwise = saucerBullet $ loadedPictures s
                standardRotation = -90

    getPosition :: Projectile -> (Float, Float)
    getPosition = projectilePosition


instance Renderable PlayerState where
    render player _ = Rotate rotation bmp
        where
            bmp = Rotate standardRotation $ Bitmap $ playerBitmapData player
            rotation = radToDeg (argV (playerFacing player))
            standardRotation = 90

    getPosition = playerPosition

instance Renderable Animation where
    render animation _ = pictureFrames animation !! onFrame animation

    getPosition = animationPosition

instance Renderable Enemy where
    render e s = case e of
            MkAsteroid{asteroidSize, asteroidSpeed} -> 
                Rotate (rotation asteroidSpeed) 
                $ Scale (asterScF asteroidSize) (asterScF asteroidSize) picAsteroid
            MkSaucer{saucerSize, saucerSpeed} -> 
                Rotate (rotation saucerSpeed) 
                $ Scale (saucerScF saucerSize) (saucerScF saucerSize) picSaucer
        where
            picSaucer@(Bitmap sBmpData) = saucerPicture $ loadedPictures s
            picAsteroid@(Bitmap aBmpData) = asteroidPicture $ loadedPictures s
            asterScF aSize = enemySize aSize / fromIntegral (fst $ bitmapSize aBmpData)
            saucerScF sSize = enemySize sSize / fromIntegral (fst $ bitmapSize sBmpData)
            enemySize size = unsafeSearch size standardSize
            rotation speed = radToDeg $ argV speed

    getPosition MkAsteroid{asteroidPosition} = asteroidPosition
    getPosition MkSaucer{saucerPosition} = saucerPosition

instance Renderable Button where
    getPosition _ = (0,0) -- must define, but will not be used

    translatedRender b _ = do
        case b of

            (MkButton (x, y) (w, h) c s) -> do
                width <- stringWidth Roman s
                let offset = fromIntegral $ negate $ width `div` 4
                    offsety = -20
                    sF = 0.5

                return $ Translate x y $ Pictures [
                        Color c $ rectangleWire w h,
                        Translate offset offsety $ Scale sF sF $ Color white $ Text s
                    ]

-- Picture button is automatically square with size 100x100
            (MkPicButton (x, y) c pic) -> return $ Translate x y $ Pictures [
                                        Color c $ rectangleWire size size,
                                        pic
                                    ] where size = 100


instance (Renderable a) => Renderable [a] where
  getPosition _ = (0,0) -- must define, but will not be used

  translatedRender lst s = do
    pics <- mapM (`translatedRender` s) lst
    return $ Pictures pics