module State where

import Player
import Imports
import Projectile
import Animation
import Enemy
import HandleInputs
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import qualified Data.Set as S
import Pausing


data State = State {    -- All positions of the State will be defined in a 16:9 field, 
                        -- maybe 720p (1280x720) to create easy conversion on HD screens.
                        enemies :: [Enemy],
                        projectiles :: [Projectile],
                        animations :: [Animation],
                        playerState :: PlayerState,
                        score :: Int,
                        timePlayed :: Float,
                        gameLoop :: GameLoop,
                        inputs :: Inputs,
                        downKeys :: S.Set Key,
                        mousePosition :: (Float, Float)
                        }
                            deriving (Show, Eq)

standardState :: IO State
standardState = do
    Right playerBMP <- readBMP "images\\ship32.bmp"
    let playerBMPData = bitmapDataOfBMP playerBMP
    return $ State {
            enemies = [],
            projectiles = [], --Projectile (500, 360) (0,0) 10],
            animations = [mkDeathAnimation (600, 360)],
            playerState = PlayerState {
                playerPosition = (640, 360),
                playerFacing = normalizeV (1,0),
                playerSpeed = (0,0),
                playerAcceleration = (0,0),
                playerLives = 3,
                playerReloadTime = 0,
                playerBitmapData = playerBMPData
            },
            score = 0,
            timePlayed = 0,
            gameLoop = Running,
            inputs = standardInputs,
            downKeys = S.empty,
            mousePosition = (0, 0)
}

stepProjectiles :: State -> State
stepProjectiles s = s {projectiles = mapMaybe stepProjectile (projectiles s)}

stepAnimations :: State -> State
stepAnimations s = s {animations = mapMaybe stepAnimation (animations s)}

playerDies :: State -> State
playerDies s = s -- make bitmap go into pieves and explode it like that

shootFromPlayer :: State -> State
shootFromPlayer s | playerReloadTime (playerState s) > 0 = s
                  | otherwise = s {projectiles = Projectile {
                                    projectilePosition = playerPosition $ playerState s, -- PMath.+ 2 PMath.* playerFacing s,
                                    projectileSpeed = playerSpeed (playerState s) PMath.+ (50 PMath.* playerFacing (playerState s)),
                                    projectileTimeAlive = 20
                                    } : projectiles s,
                                playerState = (playerState s) {playerReloadTime = 5} }

shootFromSaucer :: Enemy -> Projectile
shootFromSaucer = undefined

mouseClick :: State -> IO State
mouseClick s = do
    a <- buttonsWithActions
    let mousePos = mousePosition s
        jeiaj = filter (isInside mousePos) a
        fea = snd $ head jeiaj
    fea s

isInside :: (Float, Float) -> (Button, State -> IO State) -> Bool
isInside (x, y) (MkButton (btnX, btnY) (w, h) _ _ , _) = isInside' x btnX w && isInside' y btnY h
isInside (x, y) (MkPicButton (btnX, btnY) _ _ , _) = isInside' x btnX 100 && isInside' y btnY 100

isInside' :: Float -> Float -> Float -> Bool
isInside' mPos btnPos len = mPos >= btnPos - halfLen && mPos <= btnPos + halfLen
    where halfLen = len / 2

buttonsWithActions :: IO [(Button, State -> IO State)]
buttonsWithActions = do
    btns <- pausingButtons
    return $ zip btns [
                        \s -> return $ s {gameLoop = OptionsMenu},
                        \s -> return $ s {gameLoop = Running},
                        \s -> return $ s {gameLoop = GameQuitted},
                        saveGame,
                        loadGame
                    ]

saveGame :: State -> IO State
saveGame = undefined

loadGame :: State -> IO State
loadGame = undefined

data GameLoop = Running | Paused | GameOver | GameQuitted | OptionsMenu
                deriving (Show, Eq, Enum)
