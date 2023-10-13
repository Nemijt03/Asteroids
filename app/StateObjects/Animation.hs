module Animation where

data Animation = DeathAnimation | SpawnAnimation
                deriving (Show, Eq)
newtype DeathAnimation = MkDeathAnimation Int
                deriving (Show, Eq)
newtype SpawnAnimation = MkSpawnAnimation Int
                deriving (Show, Eq)
-- maybe dus functie voor animation :: Int -> Picture