module HandleInputs where

import Assoc
import Imports

type Inputs = Assoc Key UserAction

data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause | None
            deriving (Eq, Show, Enum, Ord, Bounded)

standardInputs :: Inputs
standardInputs = [(Char 'a'         , TurnLeft  ), 
                (Char 'd'         , TurnRight), 
                (Char 'w'         , Forward  ),
                (Char 's'         , Backward ), 
                (SpecialKey KeyEsc, Pause    )]


pausedUserActions :: Set UserAction
pausedUserActions = fromList [Pause, None]

runningUserActions :: Set UserAction
runningUserActions = fromList [minBound..]