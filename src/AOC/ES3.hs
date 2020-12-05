module AOC.ES3 (main) where

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
        _ -> putStrLn "Part1 Error: no length defined"