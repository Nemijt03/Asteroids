module LeaderBoardLogic (getLeaderBoard, toLeaderBoard) where
import SavingAndLoading(checkExists)
import Data.Aeson
import Control.Exception
import Data.List

type Leaderboard = [(String, Int)]
type LeaderboardEntry = (String, Int)

getLeaderBoard :: IO Leaderboard
getLeaderBoard = do
    results <- mapM getScore [1 .. maxEntries]
    return $ filter (\a -> a /= ("",0)) results

getScore :: Int -> IO LeaderboardEntry
getScore int = do
    b <- checkExists $ getLeaderBoardFilePath int
    if b 
        then do
            result <- decodeFileStrict' $ getLeaderBoardFilePath int
            case result of
                Nothing      -> return ("",0)
                (Just score) -> return score
        else return ("",0)
        

putScore :: LeaderboardEntry -> Int -> IO ()
putScore tuple int = catch action handler
    where
        action = encodeFile (getLeaderBoardFilePath int) tuple
        handler :: IOException -> IO ()
        handler = \e -> return ()

toLeaderBoard :: LeaderboardEntry -> IO ()
toLeaderBoard tuple = do
    leaderBoard <- getLeaderBoard
    let newLeaderBoard = sortBy (\(_,entryScore) (_, score) -> score `compare` entryScore) (tuple : leaderBoard)
    mapM (\(t,int) -> putScore t int) $ zip newLeaderBoard [1 .. maxEntries]
    return ()

getLeaderBoardFilePath :: Int -> String
getLeaderBoardFilePath int = "leaderboard/score" ++ show int ++ ".json"

maxEntries :: Int
maxEntries = 25