{-# LANGUAGE DeriveGeneric #-}
module SavingAndLoading where
import State
import GHC.Generics
import Data.Aeson
import System.IO
import Pausing

instance ToJSON State
instance FromJson State


getFilePathToSave :: Int -> String
getFilePathToSave int = "saves/save" ++ show int ++ ".json"

getStateFromFile :: Int -> IO (Maybe State)
getStateFromFile num =  do
    a <- decodeFileStrict' getFilePathToSave num
    return a

putStateToFile :: Int -> State -> IO ()
putStateToFile num state = encodeFile (getFilePathToSave num) state

savingButtonsWithActions :: IO [(Button, State -> IO State)]
savingButtonsWithActions = do
    fileResults <- mapM (\int -> checkExists (getFilePathToSave int)) [1 .. 5] 
    let availabilityStrings = zipWith getSlotAvailability [1 .. 5] fileResults
    return $ zip (zipWith createSaveButton [1 .. 5] availabilityStrings)
                 (map actionGetSave [1 .. 5])
    where
        createSaveButton int string = mkButton (0,250 + 100 * int) (600,80) (greyN 0.4) string 
        getSlotAvailability int False = "<Slot " ++ show int ++ " Available>" 
        getSlotAvailability int True  = "save " ++ show int
        actionGetSave int = \s -> do
            putStateToFile int s
            return s

checkExists :: FilePath -> IO Bool
checkExists filePath = catch ((readFile filePath) >>= (return True)) (return False)