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
import SavingAndLoading
import Graphics.UI.GLUT (Size(Size))
import Pausing
import SavingAndLoading


stateToPicture :: State -> IO Picture
stateToPicture state =
    do
        enemiesPic <- translatedRender $ enemies state
        projectilesPic <- translatedRender $ projectiles state
        animationsPic <- translatedRender $ animations state
        player <- translatedRender $ playerState state
        
        --score <- scoreToPicture (score state)
        -- let gameLoopShow = Color (makeColorI 255 255 255 0) $ Text $ show $ gameLoop state

        pausedButtons <- getButtons buttonsWithActions
        saveButtons <- getButtons savingButtonsWithActions
        loadButtons <- getButtons loadingButtonsWithActions
        leadScores <- getLeaderBoard

        let gameLoopPictures = case gameLoop state of
                                Paused ->  [pausedButtons]
                                Saving ->  [saveButtons]
                                Loading -> [loadButtons]
                                Leaderboard -> [leadScores]
                                _ -> []
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
    where
        getButtons buttons = do
            btns <- buttons
            let btns1 = map fst btns
            retButtons <- buttonsToPicture btns1
            return retButtons

getLeaderBoard :: IO Picture
getLeaderBoard = do
    exists <- mapM (\i -> checkExists $ "/topScores/score" ++ show i) [1 .. 5]
    texts <- mapM (uncurry getLeaderScores) $ zip exists [1 .. 5]
    let scores = zipWith mkScores [1 .. 5] texts 
    buttonsToPicture scores
        where
            getLeaderScores False int = return $ "score " ++ show int ++ " missing"
            getLeaderScores True int = do
                score <- readFile $ "/topScores/score" ++ show int
                return $ show int ++ ": " ++ score
            mkScores int text = MkButton (0,250 - 90 * int) (600,80) (greyN 0.4) text





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


instance (Renderable a) => Renderable [a] where
  render _ = Blank -- must define, but will not be used
  getPosition _ = (0,0) -- must define, but will not be used
  translatedRender lst = do 
    pics <- mapM translatedRender lst
    return $ Pictures pics