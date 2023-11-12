{-# LANGUAGE DeriveGeneric #-}
{-# language NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
module SavingAndLoading where
import State
import GHC.Generics
import qualified Data.Aeson as Ae
import System.IO
import Imports
import Control.Exception
import qualified Data.Set as S
import Enemy
import Player
import Projectile
import System.Random
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


--state without things not saveable to Json via Generic or instancing: missing randomG, animations, inputs, downkeys, loadedPictures
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
            rando <- initStdGen
            standardPics <- standardPictures
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
                                    randomG         = rando,
                                    loadedPictures  = standardPics,
                                    name           = ""}
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
            (return $ Just newS)

putStateToFile :: Int -> State -> IO ()
putStateToFile num state = catch action handler 
    where
        action = Ae.encodeFile (getFilePathToSave num) (toSaveableState state)
        handler :: IOException -> IO ()
        handler = \e -> return ()

checkExists :: FilePath -> IO Bool
checkExists filePath = catch (do 
        file <- openFile filePath ReadMode
        hClose file 
        return True) 
        handler
    where
        handler :: IOException -> IO Bool
        handler = \e -> return False