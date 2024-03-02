module Lib where
import Data.Char (isPrint, isSeparator)

type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]

isEmpty :: String -> Bool
isEmpty str =
    null str
        || all (\s -> not (isPrint s) || isSeparator s) str

isNotEmpty :: String -> Bool
isNotEmpty = not . isEmpty

numberLines :: (String -> Bool) -> (String -> Bool) -> [String] -> NumberedLines
numberLines shouldIncr shouldNumber stringLines =
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        go counter (x : xs) =
            let mNumbering = if shouldNumber x then Just counter else Nothing
                newCounter = if shouldIncr x then counter + 1 else counter
            in (mNumbering, x) : go newCounter xs
    in go 1 stringLines

numberAllLines :: [String] -> NumberedLines
numberAllLines = numberLines (const True) (const True)

numberNonEmptyLines :: [String] -> NumberedLines
numberNonEmptyLines = numberLines (const True) isNotEmpty

numberAndIncrementNonEmptyLines :: [String] -> NumberedLines
numberAndIncrementNonEmptyLines = numberLines isNotEmpty isNotEmpty

-- Task #1
myUnlines :: [String] -> String
myUnlines [] = []
myUnlines (l : ls) = l ++ '\n' : myUnlines ls

myUnwords :: [String] -> String
myUnwords [] = []
myUnwords [w] = w
myUnwords (w : ws) = w ++ ' ' : myUnwords ws

-- Task #2
data PadMode = PadLeft | PadRight | PadCenter

pad :: PadMode -> Int -> String -> String
pad PadCenter n str =
    let diff = (n - length str) `div` 2
        padding = replicate diff ' '
    in padding ++ str ++ padding
pad mode n str =
    let diff = n - length str
        padding = replicate diff ' '
    in case mode of
        PadLeft -> padding ++ str
        PadRight -> str ++ padding

padLeft :: Int -> String -> String
padLeft = pad PadLeft

padRight :: Int -> String -> String
padRight = pad PadRight

padCenter :: Int -> String -> String
padCenter = pad PadCenter

-- Task #3
myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (a : as) (b : bs) = (a, b) : myZip as bs

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f = go
    where 
        go _ [] = []
        go [] _ = []
        go (a : as) (b : bs) = f a b : go as bs 