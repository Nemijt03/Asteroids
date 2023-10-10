import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss.Interface.IO.Game


data State = State {
			enemies :: [Enemy],
			projectiles :: [Projectile],
			animations :: [Animation],
			playerState :: PlayerState,
			score :: Int,
			timePlayed :: Int,
			gameLoop :: GameLoop
            }
            deriving (Show, Eq)
data PlayerState = PlayerState {
                playerPosition :: Point,
                playerFacing :: Vector, -- Normalised vector
                playerSpeed :: Vector,
                playerAcceleration :: Vector,
                playerLives :: Int,
                playerReloadTime :: Int -- will be able to shoot when at 0
                }
                deriving (Show, Eq)
data GameLoop = Running | Paused
                deriving (Show, Eq)


                deriving (Show, Eq)
handleAction :: UserAction -> State -> State
handleAction = undefined

addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration = undefined


data Enemy = Asteroid | Saucer 
                deriving (Show, Eq)
data Asteroid = MkAsteroid {
                asteroidPosition :: Point, 
                asteroidSpeed :: Vector,
                asteroidSize :: Size,
                asteroidHealth :: Int
                }
                deriving (Show, Eq)
data Saucer = MkSaucer {
                saucerPosition :: Point,
                saucerSpeed :: Vector,
                saucerAcceleration :: Vector,
                saucerSize :: Size,
                saucerHealth :: Int,
                saucerReloadTime :: Int
                }
                deriving (Show, Eq)
data Size = Small | Medium | Large | ExtraLarge
                deriving (Show, Eq)

shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer = undefined

shootFromSaucer :: Enemy -> Projectile
shootFromSaucer = undefined

data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int
                }
                deriving (Show, Eq)


removeDead :: State -> State
removeDead = undefined

data Animation = DeathAnimation | SpawnAnimation
                deriving (Show, Eq)
newtype DeathAnimation = MkDeathAnimation Int
                deriving (Show, Eq)
newtype SpawnAnimation = MkSpawnAnimation Int
                deriving (Show, Eq)
-- maybe dus functie voor animation :: Int -> Picture

stateToPicture :: State -> Picture
stateToPicture state = undefined

playerStateToPicture :: PlayerState -> Picture
playerStateToPicture ps = undefined

movePlayer :: PlayerState -> PlayerState
movePlayer state = state { playerPosition = playerPosition state PMath.+ playerSpeed state }