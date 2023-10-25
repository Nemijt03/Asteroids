module HandleInputs where

import Assoc
import Imports

type Inputs = Assoc Key UserAction

data UserAction = TurnLeft | TurnRight | Forward | Backward | Pause | QuitGame | Options | None
            deriving (Eq, Show, Enum, Ord, Bounded)

standardInputs :: Inputs
standardInputs = [
                (Char 'a'         , TurnLeft  ), 
                (Char 'd'         , TurnRight), 
                (Char 'w'         , Forward  ),
                (Char 's'         , Backward ), 
                (SpecialKey KeyEsc, Pause    ),
                (Char '0'         , QuitGame),
                (Char 'o'         , Options)
                ]


pausedUserActions :: Set UserAction
pausedUserActions = fromList [Pause, QuitGame, Options, None]

runningUserActions :: Set UserAction
runningUserActions = fromList [s | s <- [minBound..], s /= QuitGame && s /= Options]