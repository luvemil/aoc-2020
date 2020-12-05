{-# LANGUAGE TupleSections #-}

module AOC.ES1 (main) where

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [_] = []
getPairs (x : xs) = couple x xs ++ getPairs xs
  where
    couple z zs = map (z,) zs

filterBySum :: (Num a, Eq a) => a -> [(a, a)] -> [(a, a)]
filterBySum tot = filter (\(x, y) -> x + y == tot)

parseInput :: [String] -> IO [Integer]
parseInput ss = do
    let loop (x : xs) = do
            y <- readIO x
            ys <- loop xs
            pure $ y : ys
        loop [] = pure []
    loop ss

main :: FilePath -> IO ()
main input = do
    inputLines <- lines <$> readFile input
    parsed <- parseInput inputLines
    let results = filterBySum 2020 $ getPairs parsed
    case results of
        [] -> putStrLn "No pair found"
        [(x, y)] -> putStrLn $ "Result: " ++ show (x * y)
        _ -> putStrLn "Multiple matches found"
