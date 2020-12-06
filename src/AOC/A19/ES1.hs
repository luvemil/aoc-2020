module AOC.A19.ES1 (main) where

import AOC.Utils

fuelReq :: Int -> Int
fuelReq x =
    let fuel = floor (fromIntegral x / 3 :: Double) - 2
     in if fuel > 0 then fuel else 0

fuelRecursive :: Int -> Int
fuelRecursive x =
    let fuel = fuelReq x
     in if fuel > 0
            then fuel + fuelRecursive fuel
            else 0

testSet :: [Int]
testSet = [100756]

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList readIO inputLines
    let fuels = map fuelReq parsed
        total = sum fuels
    putStrLn $ "Part1 Result: " ++ show total
    let fuels2 = map fuelRecursive parsed
        total2 = sum fuels2
    putStrLn $ "Part2 Result: " ++ show (total2)