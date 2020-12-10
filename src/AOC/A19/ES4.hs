module AOC.A19.ES4 (main) where

import AOC.Utils
import Data.Char (isDigit)

import Control.Monad
import Data.Maybe (isJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

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

countOcc :: Eq a => [a] -> a -> Int
countOcc xs x = length $ filter (== x) xs

isValid2 :: String -> Bool
isValid2 s =
    isValid s
        && 2 `elem` map (countOcc s) s

testPwds :: [String]
testPwds = ["112233", "123444"]

main :: FilePath -> IO ()
main fp = do
    inputVals <- splitOn '-' <$> readFile fp
    x :: Int <- readIO (head inputVals)
    y :: Int <- readIO (inputVals !! 1)
    let values = [x .. y]
        pwds = map show values
        valid = filter isValid pwds
    putStrLn $ "Part1 result: " ++ show (length valid)
    let valid2 = filter isValid2 pwds
    putStrLn $ "Part2 result: " ++ show (length valid2)