{-# language NamedFieldPuns #-}
module SpawnEnemies (spawnEnemy) where

import State
import Enemy
import System.Random
import ScreenLogic
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath

--main export module
spawnEnemy :: State -> State
spawnEnemy s@State{timePlayed,enemies,randomG} | mod (floor timePlayed) 30 == 0 
                                                 && length enemies < maxEnemies timePlayed = --if there still is room
    let (r1, g') = random randomG in--random between 0 and 1

        case compare (spawnChance timePlayed) r1 of --if it will spawn one
            LT -> s{randomG = g'}
            _  -> let (r2, g'') = random randomG in

                    case compare (enemyChance timePlayed) r2 of --which one it will spawn
                        GT  -> let (saucer, lastg) = makeRandomSaucer g'' in
                                s{enemies = saucer : enemies, randomG = lastg }

                        _   -> let (asteroid, lastg) = makeRandomAsteroid g'' in
                                s{enemies = asteroid : enemies, randomG = lastg }
                                                | otherwise                   = s


--limits and chances

maxEnemies :: Float -> Int --minimum of 5 at a time, with a maximum of 20. every 600 ticks, increase it by 1.
maxEnemies time = min 20 (5 + floor (time / 600))

spawnChance :: Float -> Float --minimum of 10%, with a maximum of 80%, every 300 ticks, increase the chance by 5%
spawnChance time = min 0.8 (0.1 + fromInteger(floor(time / 300)) * 0.05)

enemyChance :: Float -> Float --start at 0%, with a maximum of 70%. Every 300 ticks, increase the chance by 4%
enemyChance time = min 0.7 (fromInteger(floor (time / 600)) * 0.04)


--randomSpawn

makeRandomAsteroid :: StdGen -> (Enemy, StdGen)
makeRandomAsteroid g =
    let
        (randomSpeed, g')       = getRandomSpeed 7 g
        (randomPosition, g'')   = getRandomPoint randomSpeed g'
        (randomHealth,g''')     = randomR (1,4) g''
        (randomSize,lastG)      = getRandomSize g'''
    in
        (MkAsteroid{
        asteroidPosition    = wrap randomPosition,
        asteroidHealth      = randomHealth,
        asteroidSpeed       = randomSpeed,
        asteroidSize        = randomSize
        }
        ,lastG)


makeRandomSaucer :: StdGen -> (Enemy,StdGen)
makeRandomSaucer g =
    let
        (randomSpeed, g')       = getRandomSpeed 7 g
        (randomPosition, g'')   = getRandomPoint randomSpeed g'
        (randomHealth,g''')     = randomR (2,7) g'' :: (Int, StdGen)
        (randomSize,lastG)      = getRandomSize g'''
    in
    (MkSaucer{
        saucerAcceleration = (0,0),
        saucerHealth        = randomHealth,--randomHealth,
        saucerPosition      = randomPosition,
        saucerReloadTime    = 0,
        saucerSize          = randomSize,
        saucerSpeed         = randomSpeed
    }
    ,lastG)

--random value functions

getRandomSpeed :: Float -> StdGen -> (Vector, StdGen)
getRandomSpeed mag g = let (spx,  r1) = randomR (-mag,mag) g
                           (spy,  r2) = randomR (-mag,mag) r1
                        in ((spx, spy), r2)

--the entrance of the object is on the edges, so we use the speed to see which side it should go
getRandomPoint :: Vector -> StdGen -> (PMath.Point, StdGen)
getRandomPoint (xSpeed, ySpeed) g =
    case (signum xSpeed, signum ySpeed) of
        (1,-1)  -> let (x,r) = randomR (0,screenWidth) g
                    in ((x,          0),             r)
        (-1,1)  -> let (y,r) = randomR (0,screenHeight) g
                    in ((screenWidth,y),             r)
        (-1,-1) -> let (x,r) = randomR (0,screenWidth) g
                    in ((x,           screenHeight), r)
        _       -> let (y,r) = randomR (0,screenHeight) g --one of the vec mag is zero
                    in ((0,          y),             r)

getRandomSize :: StdGen -> (EnemySize,StdGen)
getRandomSize g = let (r, g') = randomR (0,100) g  :: (Int, StdGen) in
                  let size  | r < 5     = ExtraLarge --5%  chance
                            | r < 20    = Large      --15% chance
                            | r < 40    = Small      --20% chance
                            | otherwise = Medium     --60% chance
                in (size,g')

