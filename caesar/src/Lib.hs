module Lib where

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isLower :: Char -> Bool
isLower char = char `elem` lowerAlphabet

isUpper :: Char -> Bool
isUpper char = char `elem` upperAlphabet

isDigit :: Char -> Bool
isDigit char = char `elem` digits

isMisc :: Char -> Bool
isMisc char = char `notElem` lowerAlphabet ++ upperAlphabet ++ digits

indexOf :: Char -> [Char] -> Int
indexOf _ [] = undefined
indexOf ch (x : xs) = if x == ch then 0 else 1 + indexOf ch xs

alphabetRot :: Alphabet -> Int -> Char -> Char
alphabetRot alphabet n ch = 
    alphabet !! ((indexOf ch alphabet + n) `mod` length alphabet )

upperRot :: Int -> Char -> Char
upperRot = alphabetRot upperAlphabet

lowerRot :: Int -> Char -> Char
lowerRot = alphabetRot lowerAlphabet

rotChar :: Int -> Char -> Char
rotChar n ch
    | isLower ch = lowerRot n ch
    | isUpper ch = upperRot n ch
    | otherwise = ch

caesar :: Int -> String -> String
caesar n = map (rotChar n)

rot13 :: String -> String
rot13 = caesar 13

-- Task #1
at :: [a] -> Int -> a
at [] _ = undefined
at (x : xs) i
    | i < 0 = undefined
    | i == 0 = x
    | otherwise = at xs (i - 1::Int)

-- Task #2
digitRot :: Int -> Char -> Char
digitRot = alphabetRot digits

rotDigit :: Int -> Char -> Char
rotDigit n ch
    | isDigit ch = digitRot n ch
    | otherwise = ch

rot5 :: String -> String
rot5 = map (rotDigit 5)

rot135 :: String -> String
rot135 = rot13 . rot5

-- Task #3
count :: Char -> String -> Int
count _ [] = 0
count ch (x : xs) = (if ch == x then 1 else 0) + count ch xs
