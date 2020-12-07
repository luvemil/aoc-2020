module AOC.A19.ES4 (main) where

import AOC.Utils
import Data.Char (isDigit)

toPairs :: [a] -> [(a, a)]
toPairs [] = []
toPairs [_] = []
toPairs (x : x' : xs) = (x, x') : toPairs (x' : xs)

isValid :: String -> Bool
isValid s =
    all isDigit s
        && length s == 6
        && any (\(x, y) -> x == y) (toPairs s)
        && all (\(x, y) -> x <= y) (toPairs s)

main :: FilePath -> IO ()
main fp = do
    inputVals <- splitOn '-' <$> readFile fp
    x :: Int <- readIO (head inputVals)
    y :: Int <- readIO (inputVals !! 1)
    let values = [x .. y]
        pwds = map show values
        valid = filter isValid pwds
    putStrLn $ "Part1 result: " ++ show (length valid)