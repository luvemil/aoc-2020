{-# LANGUAGE TemplateHaskell #-}

module AOC.A21.ES10 where

import AOC.Utils
import AOC.Utils.Math (medianOf)
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans.Except
import qualified Data.Foldable as F
import Data.Maybe (fromJust)
import Text.Megaparsec

data Line
    = Incomplete String String -- stack of open parenthesis
    | Complete String
    | Corrupted String Char Int -- String, Found, Position
    deriving (Show, Eq)

makePrisms ''Line

lineToString :: Line -> String
lineToString (Incomplete s _) = s
lineToString (Complete s) = s
lineToString (Corrupted s _ _) = s

type LineCompState = [Char]

type LineInterpreter = StateT LineCompState (ExceptT (Char, Int) IO)

openWith :: Char -> LineInterpreter ()
openWith c = do
    xs <- get
    put $ c : xs

isOpening :: Char -> Bool
isOpening = (`elem` ("([{<" :: [Char]))

isClosing :: Char -> Bool
isClosing = (`elem` (")]}>" :: [Char]))

closingChar :: Char -> Maybe Char
closingChar x = (")]}>" :: [Char]) ^? traversed . filtered (`closes` x)

closes :: Char -> Char -> Bool
')' `closes` '(' = True
']' `closes` '[' = True
'}' `closes` '{' = True
'>' `closes` '<' = True
_ `closes` _ = False

closeWith :: Char -> LineInterpreter ()
closeWith c = do
    lastOpen : es <- get
    if c `closes` lastOpen
        then do
            put es
            pure ()
        else lift . except $ Left (c, length es)

insertChar :: Char -> LineInterpreter ()
insertChar c
    | isOpening c = openWith c
    | otherwise = closeWith c

fullLine :: String -> LineInterpreter Line
fullLine s = do
    mapM_ insertChar s
    es <- get
    case length es of
        0 -> pure $ Complete s
        _ -> pure $ Incomplete s es

computeLine :: String -> IO Line
computeLine s = do
    res <- runExceptT $ evalStateT (fullLine s) []
    case res of
        Left (expected, pos) -> pure $ Corrupted s expected pos
        Right li -> pure li

lineParser :: Parser String
lineParser = many parensParser
  where
    parensParser = oneOf ("()[]{}<>" :: [Char])

_string :: Fold Line Char
_string = folding lineToString

evalChar :: Char -> Int
evalChar ')' = 3
evalChar ']' = 57
evalChar '}' = 1197
evalChar '>' = 25137
evalChar c = error $ "Unexpected closing char " ++ [c]

evalChar2 :: Char -> Int
evalChar2 ')' = 1
evalChar2 ']' = 2
evalChar2 '}' = 3
evalChar2 '>' = 4
evalChar2 c = error $ "Unexpected closing char " ++ [c]

evalCorruptedLine :: Line -> Int
evalCorruptedLine (Corrupted _ c _) = evalChar c
evalCorruptedLine l = error $ "Unexpected line type: " ++ show l

evalIncompleteLine :: Line -> Int
evalIncompleteLine (Incomplete _ es) = F.foldl' (\acc cur -> acc * 5 + (evalChar2 . fromJust . closingChar) cur) 0 es
evalIncompleteLine l = error $ "Unexpected line type: " ++ show l

part1 :: [Line] -> IO Int
part1 ls = do
    let corr = ls ^.. traversed . filteredBy _Corrupted
        res = sumOf (traversed . filteredBy _Corrupted . to evalCorruptedLine) ls
    -- forM_ corr $ print
    pure res

part2 :: [Line] -> IO Int
part2 ls = do
    let res = medianOf (traversed . filteredBy _Incomplete . to evalIncompleteLine) ls
    embedMaybe res

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    validated <- mapM (embedMaybe . parseMaybe lineParser) inputLines
    allLines <- mapM computeLine validated
    res1 <- part1 allLines
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 allLines
    putStrLn $ "Res2: " ++ show res2