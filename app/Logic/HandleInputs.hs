module HandleInputs where

import Assoc
import Graphics.Gloss.Interface.IO.Game

type Inputs = Assoc Key UserAction

data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause
            deriving (Eq, Show)

standardInputs :: Inputs
standardInputs = [(Char 'a'         , TurnLeft  ), 
                (Char 'd'         , TurnRight), 
                (Char 'w'         , Forward  ),
                (Char 's'         , Backward ), 
                (SpecialKey KeyEsc, Pause    )]

