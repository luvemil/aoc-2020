module AOC.ES9 (main) where

-- isValid preamble -> array -> boolean
import AOC.Utils
import Data.Function ((&))

getValidity :: Int -> [Int] -> [(Int, Bool)]
getValidity x xs =
    let items = take (x + 1) xs
     in if length items == x + 1
            then
                let (base, y : _) = splitAt x xs
                    ps = map (uncurry (+)) $ getPairs base
                    val = y `elem` ps
                 in (y, val) : getValidity x (tail xs)
            else []

allSublists :: [a] -> [[a]]
allSublists [] = [[]]
allSublists z@(_ : xs) = z : allSublists xs

anySublist :: ([a] -> Bool) -> [a] -> Bool
anySublist f [] = f []
anySublist f z@(_ : xs) = f z || anySublist f xs

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let parsed :: [Int] = map read inputLines
        vals = getValidity 25 parsed
        res1 = head (filter (not . snd) vals)
    putStrLn $ "Part1 result: " ++ show res1
    let toTest =
            takeWhile snd vals
                & map fst
                & allSublists
        valToSum = fst res1
        res2 =
            toTest
                & map reverse
                & filter (anySublist ((== valToSum) . sum))
                & head
                & allSublists
                & filter ((== valToSum) . sum)
                & head
    putStrLn $ "Part2 result: " ++ show (minimum res2 + maximum res2)