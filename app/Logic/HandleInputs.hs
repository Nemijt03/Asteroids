{-# LANGUAGE DeriveGeneric #-}
module HandleInputs where


import Assoc
import Imports
import qualified Data.Set as S
import GHC.Generics
import qualified Data.Aeson as Ae

instance Ae.ToJSON UserAction
instance Ae.FromJSON UserAction

type Inputs = Assoc Key UserAction

-- all different actions an event can trigger.
data UserAction = TurnLeft | TurnRight | Forward | Backward | Shoot | Pause | TriggerQuitGame | TriggerOptions | None
            deriving (Eq, Show, Enum, Ord, Bounded, Generic)

-- the standard inputs to be put in the standardState
standardInputs :: Inputs
standardInputs = [
                (Char 'a'               , TurnLeft  ), 
                (Char 'd'               , TurnRight), 
                (Char 'w'               , Forward  ),
                (Char 's'               , Backward ), 
                (SpecialKey KeySpace    , Shoot ), 
                (SpecialKey KeyEsc      , Pause ),
                (Char '0'               , TriggerQuitGame),
                (Char 'o'               , TriggerOptions)
                ]

-- the specific useractions to be executed while not running.
pausedUserActions :: S.Set UserAction
pausedUserActions = S.fromList [Pause, TriggerQuitGame, TriggerOptions, None]

-- the specific useractions to be executed while running.
runningUserActions :: S.Set UserAction
runningUserActions = S.fromList [s | s <- [minBound..], s /= TriggerQuitGame && s /= TriggerOptions]
