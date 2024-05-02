
module Lib
    (
        readLogs,
        getLogsLength,
        filterLogs,
        searchLog
    ) where

import Data.Text.Internal.Search (indices)
import qualified Data.Text as T

readLogs :: String -> IO [String]
readLogs path = do
    logs <- readFile path

    return $ lines logs

filterLogs :: String -> String -> IO [String]
filterLogs path process = do
    logs <- readFile path
    
    return $ filter (\line -> length (searchLog process line) > 0) (lines logs)

searchLog :: String -> String -> [Int]
searchLog process line = indices (T.pack process) (T.pack line)

getLogsLength :: IO [String] -> IO (Maybe Int)
getLogsLength packedLogs = do
    logs <- packedLogs
    return $ case length logs of
        0 -> Nothing
        value -> Just value 