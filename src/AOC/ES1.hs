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

getTriples :: [a] -> [(a, a, a)]
getTriples [] = []
getTriples [_] = []
getTriples [_, _] = []
getTriples (x : xs) =
    triples x (getPairs xs)
        ++ getTriples xs
  where
    triples z = map (\(s, t) -> (z, s, t))

filterBySum3 :: (Ord a, Num a) => a -> [(a, a, a)] -> [(a, a, a)]
filterBySum3 tot = filter (\(x, y, z) -> x + y + z == tot)

main :: FilePath -> IO ()
main input = do
    inputLines <- lines <$> readFile input
    parsed <- parseInput inputLines
    let results = filterBySum 2020 $ getPairs parsed
    putStrLn "First Part:"
    case results of
        [] -> putStrLn "No triple found"
        [(x, y)] -> putStrLn $ "Result: " ++ show (x * y)
        _ -> putStrLn "Multiple matches found"

    let triples = getTriples parsed
        res2 = filterBySum3 2020 triples

    putStrLn "Second Part:"
    putStrLn $ "n. of elements: " ++ show (length parsed)
    putStrLn $ "Found triples: " ++ show (length triples)

    case res2 of
        [] -> putStrLn "No triple found"
        [(x, y, z)] -> putStrLn $ "Result: " ++ show (x * y * z)
        e -> putStrLn $ "Multiple matches found: " ++ show e