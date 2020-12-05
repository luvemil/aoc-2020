module AOC.ES2 (main) where

import AOC.Utils (parseList)
import Control.Exception (onException)
import Text.Regex.TDFA

data PWDEntry = PWDEntry
    { min :: Int
    , max :: Int
    , char :: Char
    , pwd :: String
    }
    deriving (Show, Eq)

isValid :: PWDEntry -> Bool
isValid PWDEntry{..} = c >= min && c <= max
  where
    c = length matching
    matching = filter (== char) pwd

xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

matchAt :: Char -> Int -> String -> Bool
matchAt c i s = (s !! i) == c

isValid2 :: PWDEntry -> Bool
isValid2 PWDEntry{..} = a `xor` b
  where
    l = length pwd
    a = (min <= l) && matchAt char (min - 1) pwd
    b = (max <= l) && matchAt char (max - 1) pwd

type Match = (String, String, String, [String])

lineRegex :: String
lineRegex = "^([0-9]+)-([0-9]+) ([a-z]): ([a-z]+)$"

parseInput :: [String] -> IO [PWDEntry]
parseInput = parseList $ \s -> onException (action s) (echo s)
  where
    echo x = do
        putStrLn $ "Failed to parse " ++ x
        fail $ "Failed to parse " ++ x
    action x = do
        (_, _, _, [minMatch, maxMatch, charMatch, pwdMatch]) :: Match <- x =~~ lineRegex
        min <- readIO minMatch
        max <- readIO maxMatch
        let char = head charMatch
            pwd = pwdMatch
        pure $ PWDEntry{..}

main :: FilePath -> IO ()
main input = do
    inputLines <- lines <$> readFile input
    parsed <- parseInput inputLines
    let result = filter isValid parsed
    putStrLn $ "Result: " ++ show (length result) ++ " valid passwords"
    let res2 = filter isValid2 parsed
    putStrLn $ "Phase 2 result: " ++ show (length res2) ++ " valid passwords"