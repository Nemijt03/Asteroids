{-# language NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use infix" #-}
module Collision (naiveCollision,removeDead,isDead, isCollision, collide) where

import Assoc
import Player
import Enemy
import Projectile
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss.Data.Point

naiveCollision :: [Enemy] -> [Projectile] -> PlayerState -> ([Enemy],[Projectile],PlayerState)
naiveCollision enemies projectiles playerState =
    let (newE,newPr)       = collideListWithList enemies projectiles
        (newPl, newerE)    = collideAll playerState newE
        newestE            = collideList newerE --all enemy collisions done
        (newerPl, newerPr) = collideAll newPl newPr
        newestPr           = collideList newerPr
    in
        (newestE,newestPr,newerPl)



collideListWithList :: (Collidable a, Collidable b) => [a] -> [b] -> ([a], [b])
collideListWithList listA listB =  helper listA listB []
    where
        helper []     ys xs  = (xs,ys)
        helper (x:xs) ys acc = let (newx,newys) = collideAll x ys
                               in helper xs newys (newx:acc)

collideList :: Collidable a => [a] -> [a]
collideList []     = []
collideList [x]    = [x]
collideList (x:xs) = let (newx, newxs) = collideAll x xs
                     in newx : collideList newxs -- very hard to do with higher-order functions.


collideAll :: (Collidable a, Collidable b) => a -> [b] -> (a,[b])
collideAll a = foldr collideAll' (a,[])
    where
        collideAll' b (a',xs) = let (newa,newb) = collide a' b in (newa,newb:xs)

intersect :: Square -> Square -> Bool
intersect (Sq (x,y) w) (Sq (x',y') w') = x < (x' + w') && (x + w) > x'
                                    && y < (y' + w') && (y + w) > y'

inside :: Square -> Square -> Bool
inside (Sq p w) (Sq p' w') = inBox tl && inBox tr && inBox dl && inBox dr
    where
        inBox = pointInBox p' (p' PMath.+ (w',w'))
        tl = p
        tr = p PMath.+ (w, 0)
        dl = p PMath.+ (0, w)
        dr = p PMath.+ (w, w)


isCollision :: (Collidable a, Collidable b) => a -> b -> Bool
isCollision a b = canDamage a && canDamage b && intersect (toSquare a) (toSquare b) 

collide :: (Collidable a, Collidable b) => a -> b -> (a,b)
collide a b | isCollision a b = (doDamage a (getDamage b), doDamage b (getDamage a)) --can be changed if a or b have a specific damage value added later
            | otherwise       = (a, b)

type Width  = Float
data Square = Sq PMath.Point Width

class Damageable a where
    doDamage :: a -> Int -> a
    isDead :: a -> Bool
    canDamage :: a -> Bool 
    canDamage _ = True -- standardly, a damageable can damage others.
    getDamage :: a -> Int
    getDamage _ = 1 -- standard 1 damage per hit.

class Squarable a where
    toSquare :: a -> Square

class (Squarable a, Damageable a) => Collidable a

--instances to damage the different objects in the game
instance Damageable Enemy where
    doDamage enemy@(MkAsteroid{asteroidHealth}) damage = enemy{asteroidHealth = asteroidHealth - damage}
    doDamage enemy@(MkSaucer  {saucerHealth})   damage = enemy{saucerHealth   = saucerHealth   - damage}

    isDead MkAsteroid{asteroidHealth} = asteroidHealth <= 0
    isDead MkSaucer{saucerHealth}     = saucerHealth <= 0


instance Damageable PlayerState where
    doDamage player@PlayerState{playerLives} damage = player{playerLives = playerLives - damage}

    isDead PlayerState{playerLives} = playerLives <= 0

    getDamage _ = let playerDamage = 5 in playerDamage


instance Damageable Projectile where
    doDamage projectile _ = projectile{projectileTimeAlive = 0}

    isDead Projectile{projectileTimeAlive} = projectileTimeAlive <= 0

    canDamage Projectile{projectileImmuneTime} = projectileImmuneTime <= 0   
     

--instances to convert the different objects to squares
instance Squarable Enemy where
    toSquare (MkAsteroid{asteroidPosition,asteroidSize}) = Sq asteroidPosition (unsafeSearch asteroidSize standardSize)
    toSquare (MkSaucer  {saucerPosition,  saucerSize})   = Sq saucerPosition   (unsafeSearch saucerSize   standardSize)

instance Squarable PlayerState where
    toSquare PlayerState{playerPosition} = Sq playerPosition (unsafeSearch Medium standardSize)

instance Squarable Projectile where
    toSquare Projectile{projectilePosition} = Sq projectilePosition projectileSize
        where projectileSize = 10


--making all objects an instance of collidable thanks to them already being Squarable and Damageable
instance Collidable Enemy

instance Collidable PlayerState

instance Collidable Projectile

--handy function
removeDead :: Damageable a => [a] -> [a]
removeDead = foldr f []
    where f x xs = if isDead x then xs else x:xs