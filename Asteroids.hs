import Graphics.Gloss


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
                position :: Point,
                facing :: Vec2, -- Normalised vector
                speed :: Vec2,
                acceleration :: Vec2,
                lives :: Int,
                reloadTime :: Int -- will be able to shoot when at 0
                }
data GameLoop = Running | Paused

data Input = Assoc Char UserAction
data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause
handleAction :: UserAction -> State -> State
handleAction = undefined

addAcceleration :: Float -> PlayerState -> PlayerState
addAcceleration = undefined


data Enemy = Asteroid | Saucer
data Asteroid = Asteroid {
                position :: Point, 
                speed :: Vec2,
                size :: Size,
                health :: Int
                }
data Saucer = Saucer {
                position :: Point,
                speed :: Vec2,
                acceleration :: Vec2,
                size :: Size,
                health :: Int,
                reloadTime :: Int
                }
data Size = Small | Medium | Large | ExtraLarge

shootFromPlayer :: PlayerState -> Projectile
shootFromSaucer :: Saucer -> Projectile

data Projectile = Projectile {
                    location :: Point
                    speed :: Vec2
                    timeAlive :: Int
                    }


removeDead :: State -> State
removeDead = undefined

data Animation = DeathAnimation | SpawnAnimation
data DeathAnimation = DeathAnimation Int Picture
data SpawnAnimation = SpawnAnimation Int Picture
-- maybe dus functie voor animation :: Int -> Picture

