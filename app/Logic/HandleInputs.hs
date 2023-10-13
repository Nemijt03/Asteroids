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

search :: Key -> Inputs -> UserAction
search key list | null userActions = None
                | otherwise = head userActions
                where
                    userActions = [x | (y,x) <- list, y == key]

updateInputs :: Key -> UserAction -> Inputs -> Inputs
updateInputs newKey ua = foldr f e
    where
        f (oldKey, ua1) xs | ua1 == ua   = (newKey, ua1) : xs
                           | otherwise   = (oldKey, ua1) : xs 
        e = []


data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause | None
            deriving (Eq, Show)