module AOC.A21.ES1 (main) where

import AOC.Utils (parseList)

-- could be done inside State [Int] Int
accumulate :: ([Int], Int) -> ([Int], Int)
accumulate (x : x' : xs, y) = accumulate (x' : xs, y')
  where
    y' =
        if x' - x > 0
            then y + 1
            else y
accumulate (_, y) = ([], y)

accumulate3 :: ([Int], Int) -> ([Int], Int)
accumulate3 (w : x : y : z : xs, res) = accumulate3 (x : y : z : xs, res')
  where
    res' =
        if z - w > 0
            then res + 1
            else res
accumulate3 (_, res) = ([], res)

main :: FilePath -> IO ()
main input = do
    inputLines <- lines <$> readFile input
    distances :: [Int] <- parseList readIO inputLines
    let (_, incCount) = accumulate (distances, 0)
    putStrLn $ "Res1: " ++ show incCount
    let (_, incCount2) = accumulate3 (distances, 0)
    putStrLn $ "Res2: " ++ show incCount2