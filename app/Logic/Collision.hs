{-# language NamedFieldPuns #-}
module Collision (doCollision) where

import State
import Assoc
import Player
import Enemy
import Projectile
import qualified Graphics.Gloss.Data.Point.Arithmetic as PMath
import Graphics.Gloss.Data.Point

--main export module
doCollision :: State -> State
doCollision s@State{enemies, projectiles, playerState} = let (newE, newPr, newPl) = naiveCollision enemies projectiles playerState
                                                     in s{enemies = newE, projectiles = newPr, playerState = newPl}

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
                     in newx:(collideList newxs) --cannot think how to do this with foldr or something.


collideAll :: (Collidable a, Collidable b) => a -> [b] -> (a,[b])
collideAll a = foldr f (a,[])
    where 
        f b (a',xs) = let (newa,newb) = collide a' b in (newa,newb:xs)
                                                     
type Width  = Float
data Square = Sq PMath.Point Width

class Damagable a where
    doDamage :: a -> Int -> a

class Squarable a where
    toSquare :: a -> Square

intersect :: Square -> Square -> Bool
intersect (Sq (x,y) w) (Sq (x',y') w') = x < (x' + w') && (x + w) > x
                                    && y < (y' + w') && (y + w) > y
inside :: Square -> Square -> Bool
inside (Sq p w) (Sq p' w') = inBox tl && inBox tr && inBox dl && inBox dr
    where
        inBox = pointInBox p' (p' PMath.+ (w',w'))
        tl = p
        tr = p PMath.+ (w, 0)
        dl = p PMath.+ (0, w)
        dr = p PMath.+ (w, w)

class (Squarable a, Damagable a) => Collidable a

isCollision :: (Collidable a, Collidable b) => a -> b -> Bool
isCollision a b = intersect (toSquare a) (toSquare b)

collide :: (Collidable a, Collidable b) => a -> b -> (a,b)
collide a b | isCollision a b = (doDamage a 1, doDamage b 1) --can be changed if a or b have a specific damage value added later
            | otherwise       = (a, b)

--instances to damage the different objects in the game
instance Damagable Enemy where
    doDamage enemy@(MkAsteroid{asteroidHealth}) damage = enemy{asteroidHealth = asteroidHealth - damage}
    doDamage enemy@(MkSaucer  {saucerHealth})   damage = enemy{saucerHealth   = saucerHealth   - damage}

instance Damagable PlayerState where
    doDamage player@PlayerState{playerLives} damage = player{playerLives = playerLives - damage}

instance Damagable Projectile where
    doDamage projectile _ = projectile{projectileTimeAlive = 0}

--instances to convert the different objects to squares
instance Squarable Enemy where
    toSquare (MkAsteroid{asteroidPosition,asteroidSize}) = Sq asteroidPosition (unsafeSearch asteroidSize standardSize)
    toSquare (MkSaucer  {saucerPosition,  saucerSize})   = Sq saucerPosition   (unsafeSearch saucerSize   standardSize)

instance Squarable PlayerState where
    toSquare PlayerState{playerPosition} = Sq playerPosition (unsafeSearch Medium standardSize)

instance Squarable Projectile where
    toSquare Projectile{projectilePosition} = Sq projectilePosition 1 --1 pixel big


--making all objects an instance of collidable thanks to them already being Squarable and Damagable
instance Collidable Enemy 

instance Collidable PlayerState

instance Collidable Projectile


--main idea: steal the quadtree idea from the midterm. Split it into 4 different sections and insert each collidable object. 
--Then only check each list which every single one in each list, such that we only have to check a few object with each other, rather
--than each object with every other object.
--so, for each node, there are 4 child nodes (generated as needed). First we have the top node, which is the entire screen.
--all belows all quarters. The object is only stored in a quarter which can wholly contain it.
--(so those on direct lines or in the center are just screwed)
--Then we go from the top of the tree to the bottom, doing collision in the own lists at the node depth and each child.
--This should heavily reduce the amount of checks needed to be done, the deeper you go, as you potentially quarter each check.
--only overhead is the creation of the tree itself.

--problems: 1 wrapping may land a single object in 2 zones, perhaps even more
--2 we have to store the objects, but we have to do so in a way which we can recall them and change them
--3 an intersection may be counted twice, as each object at an intersection, is also intersected by the other object.

--solutions: 1
--wrapping is difficult, but not impossible to deal with. The complex part is the representation in the tree, not the collision itself.
--we could simply store any wrapped object at the top, or multiple parts of the tree. Depends on which is easier to implement
--2 we could create an abstraction, or we could map each with a specific id, which correlates to an inherent consictent system.
--we could take the player first, the enemies, then projectiles. ID 1 would then be the player and the rest would be dependant on the list.
--3 remove the object from the list after the intersections are done for that element
{-
data Quadtree = Leaf [Int] --empty list denotes an empty leaf
               | ParentNode [Int] Quadtree Quadtree Quadtree Quadtree
    deriving (Eq, Show)


insertQuadTree :: Int -> Quadtree -> Quadtree
insertQuadTree f (Leaf list)                   = undefined
insertQuadTree f (ParentNode list sl sr ul ur) = undefined
-}