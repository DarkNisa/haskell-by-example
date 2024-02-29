module Main (main) where

import System.Environment ( getProgName, getArgs ) 

printHelpText :: String -> IO ()
printHelpText msg = do
    putStrLn $ msg ++ "\n"
    progName <- getProgName
    putStrLn $ "Usage: " ++ progName ++ " <filename>"

parseArguments :: [String] -> Maybe FilePath
parseArguments [filePath] = Just filePath
parseArguments _ = Nothing

main :: IO ()
main = do
    cliArgs <- getArgs
    let mFilePath = parseArguments cliArgs
    maybe (printHelpText "Missing filename") putStrLn mFilePath

-- Task #1
indexOf :: Char -> [Char] -> Maybe Int
indexOf _ [] = Nothing
indexOf ch (x : xs) = if x == ch then Just 0 else fmap (+ 1) (indexOf ch xs)