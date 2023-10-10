import Graphics.Gloss
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath


data State = State {
			enemies :: [Enemy],
			projectiles :: [Projectile],
			animations :: [Animation],
			playerState :: PlayerState,
			score :: Int,
			timePlayed :: Int,
			gameLoop :: GameLoop
            }
data PlayerState = PlayerState {
                playerPosition :: Point,
                playerFacing :: Vector, -- Normalised vector
                playerSpeed :: Vector,
                playerAcceleration :: Vector,
                playerLives :: Int,
                playerReloadTime :: Int -- will be able to shoot when at 0
                }
data GameLoop = Running | Paused

data Input = Assoc Char UserAction
data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause
handleAction :: UserAction -> State -> State
handleAction = undefined

addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration = undefined


data Enemy = Asteroid | Saucer 
data Asteroid = MkAsteroid {
                asteroidPosition :: Point, 
                asteroidSpeed :: Vector,
                asteroidSize :: Size,
                asteroidHealth :: Int
                }
data Saucer = MkSaucer {
                saucerPosition :: Point,
                saucerSpeed :: Vector,
                saucerAcceleration :: Vector,
                saucerSize :: Size,
                saucerHealth :: Int,
                saucerReloadTime :: Int
                }
data Size = Small | Medium | Large | ExtraLarge

shootFromPlayer :: PlayerState -> Projectile
shootFromPlayer = undefined

shootFromSaucer :: Enemy -> Projectile
shootFromSaucer = undefined

data Projectile = Projectile {
                projectilePosition :: Point,
                projectileSpeed :: Vector,
                projectileTimeAlive :: Int
                }


removeDead :: State -> State
removeDead = undefined

data Animation = DeathAnimation | SpawnAnimation
data DeathAnimation = MkDeathAnimation Int Picture
data SpawnAnimation = MkSpawnAnimation Int Picture

-- maybe dus functie voor animation :: Int -> Picture

stateToPicture :: State -> Picture
stateToPicture state = undefined

playerStateToPicture :: PlayerState -> Picture
playerStateToPicture ps = undefined

movePlayer :: PlayerState -> PlayerState
movePlayer state = state { playerPosition = playerPosition state PMath.+ playerSpeed state }