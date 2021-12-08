module AOC.A21.ES7 where

import AOC.Utils (Parser, arrParser, embedMaybe, intParser)
import AOC.Utils.Math
import Control.Lens
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Megaparsec
import Text.Megaparsec.Char

crabPosParser :: Parser [Int]
crabPosParser = arrParser (char ',') intParser <* eof

getFuelConsump :: [Int] -> Int -> Int
getFuelConsump xs x' = sum [abs (x - x') | x <- xs]

getFuelConsump2Single :: Int -> Int -> Int
getFuelConsump2Single x x' =
    let n = abs (x - x')
        tot = fromIntegral $ (1 + n) * n
     in round @Double $ tot / 2

getFuelConsump2 :: [Int] -> Int -> Int
getFuelConsump2 xs x' = sum [getFuelConsump2Single x x' | x <- xs]

solution :: ([Int] -> Int -> Int) -> [Int] -> IO Int
solution cost xs = do
    let minX = fromJust $ minimumOf traversed xs
        maxX = fromJust $ maximumOf traversed xs
        xs' = [minX .. maxX]
        x' = fromJust $ minimumByOf traversed (comparing (cost xs)) xs'
        fc = cost xs x'
    putStrLn $ "Got target pos: " ++ show x'
    pure fc

part1 :: [Int] -> IO Int
part1 = solution getFuelConsump

part2 :: [Int] -> IO Int
part2 = solution getFuelConsump2

main :: FilePath -> IO ()
main fp = do
    input <- readFile fp
    crabPos <- embedMaybe . parseMaybe crabPosParser $ input
    res1 <- part1 crabPos
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 crabPos
    putStrLn $ "Res2: " ++ show res2