module HandleInputs where

import Graphics.Gloss.Interface.IO.Game

type Assoc k v  = [(k, v)]
type Inputs = Assoc Key UserAction

standardInputs :: Inputs
standardInputs = [(Char 'a'         , TurnLeft  ), 
                (Char 'd'         , TurnRight), 
                (Char 'w'         , Forward  ),
                (Char 's'         , Backward ), 
                (SpecialKey KeyEsc, Pause    )]

search :: Key -> Inputs -> Maybe UserAction --if the key is not in Inputs, it should return nothing.
search c list = foldr f Nothing
    where
        f (k, u) may = case may of
            Nothing -> if c == k then Just c else Nothing
            _       -> may

updateInputs :: Key -> UserAction -> Inputs -> Inputs
updateInputs c u = foldr f e
    where
        f (x, y) xs | y == u    = (c,y) : xs
                    | otherwise = (x,y) : xs 
        e = []
data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause
            deriving (Eq, Show)