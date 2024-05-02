{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.FilePath (joinPath)
import System.Directory (getDirectoryContents)
import Data.List.Split (splitOn)
import System.Console.Haskeline
import System.Console.ANSI
import Control.Monad.IO.Class (liftIO)

import Lib (readLogs, getLogsLength)

getLogPath :: String
getLogPath = "/var/log"

colorize :: Color -> String -> String
colorize color str = setSGRCode [SetColor Foreground Vivid color] ++ str ++ setSGRCode [Reset]

proccessFile :: [String] -> String -> InputT IO ()
proccessFile (file:rest) userPath = do    
    let path = joinPath [userPath, file]

    let proccessedFile = readLogs path
    Just lengthFile <- (liftIO $ getLogsLength proccessedFile)

    outputStrLn ""
    outputStrLn $ colorize Green ("File: " ++ file) 
    outputStrLn $ colorize Green ("Lines: " ++ show lengthFile) 
    outputStrLn ""
    
    proccessFile rest userPath

proccessFile [] _ = return ()

proccessFiles :: [String] -> String -> InputT IO ()
proccessFiles files userPath = do
    proccessFile files userPath

main :: IO ()
main = runInputT defaultSettings loop
    where 
        loop :: InputT IO ()
        loop = do
            outputStrLn $ "Please, provide the log path"
            input <- getInputLineWithInitial "> " (getLogPath, "")
            case input of
                Just value -> do
                    case length value of
                        0 -> do
                            outputStrLn $ "Nothing provided. Try again."

                            loop
                        _ -> do
                            dirList <- liftIO $ getDirectoryContents value

                            mapM_ (\file -> outputStrLn $ colorize Yellow "> " ++ file) dirList

                            outputStrLn $ "Provide the file(s) path"
                            Just filesInput <- getInputLine "> "

                            let files = splitOn "," filesInput

                            proccessFiles files value