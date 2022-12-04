module AOC.A20.ES3 (main) where

-- Given the current position, returns the charac
-- getNextPos :: Int -> [String] -> (Char, Int, [String])
-- getNextPos i (s : ss) = (s !! i, i + 3, ss)

-- Our state, coordinates in (x, y) and the list of all strings

import AOC.Utils
import Control.Monad.State

-- | implementation with State
type PosState = ((Int, Int), [String])

getCurPos :: State PosState Char
getCurPos = do
    ((x, y), worldMap) <- get
    let row = worldMap !! y
    pure $ row !! x

data Result = Success | Failure

movePos :: Int -> Int -> State PosState Result
movePos x' y' = do
    ((x, y), worldMap) <- get
    let y'' = y + y'
    if y'' >= length worldMap
        then pure Failure
        else do
            let row = worldMap !! y''
                x'' = (x + x') `mod` length row
            put ((x'', y''), worldMap)
            pure Success

{- | pure implementation
 | computeSteps :: starting index -> step -> modulo -> [indexes]
-}
computeSteps :: Int -> Int -> Int -> [Int]
computeSteps i s m = i' : computeSteps i'' s m
  where
    i' = i `mod` m
    i'' = i + s `mod` m

getEach :: Int -> [a] -> [a]
getEach _ [] = []
getEach i xs@(x : _) = x : getEach i (drop i xs)

type Slope = (Int, Int)

-- right -> down -> modulo -> worldMap -> result
getTrees :: Slope -> Int -> [String] -> Int
getTrees (rightStep, downStep) modulo worldMap =
    let steps = computeSteps 0 rightStep modulo
        walkedMap = getEach downStep worldMap
        values = tail $ zipWith (!!) walkedMap steps
     in length $ filter (== '#') values

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let parsed = inputLines -- nothing to parse in this exercise
        ls = uniq $ map length parsed
    case ls of
        [l] -> do
            let steps = computeSteps 0 3 l
                values = tail $ zipWith (!!) parsed steps
                res = filter (== '#') values
            putStrLn $ "Part1 result: " ++ show (length res) ++ " trees encountered"
            let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
                res2 = map (\s -> getTrees s l parsed) slopes
            putStrLn $ "Part2 result: " ++ show (product res2)
        _ -> putStrLn "Part1 Error: no length defined"