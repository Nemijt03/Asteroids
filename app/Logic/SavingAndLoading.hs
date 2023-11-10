{-# LANGUAGE DeriveGeneric #-}
{-# language NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module SavingAndLoading (savingButtonsWithActions, loadingButtonsWithActions, checkExists) where
import State
import GHC.Generics
import qualified Data.Aeson as Ae
import Pausing
import Imports
import Control.Exception
import qualified Data.Set as S
import Enemy
import Player
import Projectile
import HandleInputs

--playerstate without bitmap data
data SaveablePlayerState = SaveablePlayerState{
                            splayerPosition :: Point,
                            splayerFacing :: Vector, -- Normalised vector
                            splayerSpeed :: Vector,
                            splayerAcceleration :: Vector,
                            splayerLives :: Int,
                            splayerReloadTime :: Float
                        } deriving (Show, Eq, Generic)


--state without things not saveable to Json via Generic or instancing: missing randomG, animations, inputs, downkeys
data SaveableState = SaveableState{
                        senemies :: [Enemy],
                        sprojectiles :: [Projectile],
                        splayerState :: SaveablePlayerState,
                        sscore :: Int,
                        stimePlayed :: Float,
                        sgameLoop :: GameLoop,
                        smousePosition :: (Float, Float),
                        soptions :: Options
                    } deriving (Show, Eq, Generic )

instance Ae.ToJSON SaveablePlayerState
instance Ae.FromJSON SaveablePlayerState
instance Ae.ToJSON SaveableState
instance Ae.FromJSON SaveableState

toSaveableState :: State -> SaveableState
toSaveableState s =
        SaveableState { senemies = enemies s ,
                        sprojectiles = projectiles s,
                        splayerState = (toSaveablePlayerState (playerState s)),
                        sscore = score s,
                        stimePlayed = timePlayed s,
                        sgameLoop = gameLoop s,
                        smousePosition = mousePosition s,
                        soptions = options s}

fromSaveableState :: SaveableState -> IO State
fromSaveableState s = 
    let p = splayerState s
        newP =  PlayerState{
            playerPosition = splayerPosition p,
            playerFacing = splayerFacing p,
            playerSpeed = splayerSpeed p  ,
            playerAcceleration = splayerAcceleration p,
            playerLives = splayerLives p,
            playerReloadTime = splayerReloadTime p
        }
        in do
            Right playerBMP <- readBMP "images\\ship32.bmp"
            let playerBMPData = bitmapDataOfBMP playerBMP
            let newPlayer = newP{playerBitmapData = playerBMPData}
            let newState =  State { enemies         = senemies s ,
                                    projectiles     = sprojectiles s,
                                    playerState     = newPlayer,
                                    score           = sscore s,
                                    timePlayed      = stimePlayed s,
                                    gameLoop        = sgameLoop s,
                                    mousePosition   = smousePosition s,
                                    options         = soptions s,
                                    animations      = [],
                                    downKeys        = S.empty,
                                    inputs          = standardInputs,
                                    randomG         = getPredictableRandom}
                in return newState

toSaveablePlayerState :: PlayerState -> SaveablePlayerState
toSaveablePlayerState p =
        SaveablePlayerState{
            splayerPosition       = playerPosition p,
            splayerFacing         = playerFacing p,
            splayerSpeed          = playerSpeed p  ,
            splayerAcceleration   = playerAcceleration p,
            splayerLives          = playerLives p,
            splayerReloadTime     = playerReloadTime p
        }



getFilePathToSave :: Int -> String
getFilePathToSave int = "saves/save" ++ show int ++ ".json"

getStateFromFile :: Int -> IO (Maybe State)
getStateFromFile num =  do
    maybeS <- Ae.decodeFileStrict' $ getFilePathToSave num
    case maybeS of 
        Nothing -> return Nothing
        Just s  -> do
            newS <- fromSaveableState s
            return $ Just $ newS

putStateToFile :: Int -> State -> IO ()
putStateToFile num state = catch (Ae.encodeFile (getFilePathToSave num) (toSaveableState state)) handler
    where
        handler :: IOException -> IO ()
        handler = \e -> return ()

savingButtonsWithActions :: IO [(Button, State -> IO State)]
savingButtonsWithActions = do
    fileResults <- mapM (\int -> checkExists (getFilePathToSave int)) [1 .. 5] --seeing what files already exists
    let availabilityStrings = zipWith getSlotAvailability [1 .. 5] fileResults
    return $ zip (zipWith createSaveButton [1 .. 5] availabilityStrings) --buttons themselves
                 (map actionPutSave [1 .. 5]) --actions with the buttons
    where
        createSaveButton int string = MkButton (0,250 - 100 * int) (600,80) (greyN 0.4) string 
        getSlotAvailability int False = "<Slot " ++ show int ++ " Available>" 
        getSlotAvailability int True  = "save " ++ show int
        actionPutSave int = \s -> do
            putStateToFile int s{gameLoop = Paused}
            return s{gameLoop = Paused}


loadingButtonsWithActions :: IO [(Button, State -> IO State)]
loadingButtonsWithActions = do
     fileResults <- mapM (\int -> checkExists (getFilePathToSave int)) [1 .. 5] --seeing what files already exists
     let availabilityStrings = zipWith getSlotAvailability [1 .. 5] fileResults
     return $ zip (zipWith createLoadButton [1 .. 5] availabilityStrings) --buttons themselves
                  (zipWith actionGetSave    [1 .. 5] fileResults) --actions with the buttons
     where 
        createLoadButton int string = MkButton (0,250 - 100 * int) (600,80) (greyN 0.4) string 
        getSlotAvailability int False = "<Save to this slot first>" 
        getSlotAvailability int True  = "save " ++ show int
        actionGetSave int False = \s -> return s
        actionGetSave int True = \s -> do
            newS <- getStateFromFile int
            case newS of
                Nothing       -> return s 
                Just newState -> return newState

checkExists :: FilePath -> IO Bool
checkExists filePath = catch (do 
        _ <- readFile filePath
        return True) 
        handler
    where
        handler :: IOException -> IO Bool
        handler = \e -> return False