module HandleInputs where

import Assoc
import Imports
import qualified Data.Set as S

type Inputs = Assoc Key UserAction

data UserAction = TurnLeft | TurnRight | Forward | Backward | Shoot | Pause | QuitGame | Options | None
            deriving (Eq, Show, Enum, Ord, Bounded)

standardInputs :: Inputs
standardInputs = [
                (Char 'a'               , TurnLeft  ), 
                (Char 'd'               , TurnRight), 
                (Char 'w'               , Forward  ),
                (Char 's'               , Backward ), 
                (SpecialKey KeySpace    , Shoot ), 
                (SpecialKey KeyEsc      , Pause ),
                (Char '0'               , QuitGame),
                (Char 'o'               , Options)
                ]


pausedUserActions :: S.Set UserAction
pausedUserActions = S.fromList [Pause, QuitGame, Options, None]

runningUserActions :: S.Set UserAction
runningUserActions = S.fromList [s | s <- [minBound..], s /= QuitGame && s /= Options]