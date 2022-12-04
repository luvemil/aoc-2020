module AOC.A20.ES10 (main) where

import AOC.Utils
import Data.Function ((&))
import Data.List (sort)

differences :: Num a => [a] -> [a]
differences (x : (x' : xs)) = (x' - x) : differences (x' : xs)
differences _ = []

isValid :: (Num a, Ord a) => [a] -> Bool
isValid (x : x' : xs)
    | x' - x <= 3 = isValid (x' : xs)
    | otherwise = False
isValid [] = True
isValid _ = False

allSublists :: [a] -> [[a]]
allSublists (x : xs) = allSublists xs ++ map (x :) (allSublists xs)
allSublists [] = [[]]

-- allSublistsThat :: ([a] -> Bool) -> [a] -> [[a]]
-- allSublistsThat f [] = [[] | f []]
-- allSublistsThat f y@(x : xs) =
--     if f y
--         then allSublistsThat f xs ++ map (x :) (allSublistsThat )

genAllLists :: (Num a, Ord a) => [a] -> [[a]]
genAllLists (x : x' : x'' : xs'')
    | x'' - x <= 3 = map (x :) (genAllLists (x' : x'' : xs'')) ++ map (x :) (genAllLists (x'' : xs''))
    | otherwise = map (x :) (genAllLists (x' : x'' : xs''))
genAllLists xs = [xs]

-- genAllListsInt :: [Int] -> [[Int]]
-- genAllListsInt [x, y, z]
--     | z - x <= 3 = [[x, y, z], [x, y], [x, z]]
--     | otherwise = [[x, y, z], [x, y]]
-- genAllListsInt [x, y, z, t]
--     | t - x <= 3 =
--         genAllListsInt [x, y, z] ++ map (++ [t]) (genAllListsInt [x, y, z]) ++ [[x, t]]
--     | otherwise = genAllListsInt [x, y, z] ++ map (++ [t]) (genAllListsInt [x, y, z])
-- genAllListsInt [x, y] = [[x, y]]
-- genAllListsInt [x] = [[x]]
-- genAllListsInt [] = [[]]

-- genAllListsInt (a : b : c : d : xs) =
--     let headList = genAllListsInt [a,b,c,d]
--     in

type PartialList a = ([a], [a])

takeNext :: PartialList Int -> [PartialList Int]
takeNext (xs, []) = [(xs, [])]
takeNext ([], y : ys) = [([y], ys)] >>= takeNext
takeNext (_, [x]) = [([x], [])]
takeNext (x : _, y : y' : ys)
    | y' - x <= 3 = [([y], y' : ys), ([y'], ys)] >>= takeNext
    | otherwise = [([y], y' : ys)] >>= takeNext

type SubList a = (Int, [a], Int)

sumPrefix :: Int -> SubList a -> SubList a
sumPrefix x (x', ys, y) = (x + x', ys, y)

sumSuffix :: Int -> SubList a -> SubList a
sumSuffix x (y, ys, x') = (y, ys, x + x')

isValidMonotoneList :: [Int] -> Bool
isValidMonotoneList xs =
    head xs == 1
        && all isIncreasing (rollingWindows 2 xs)
  where
    isIncreasing (x : x' : _)
        | x' - x >= 0 && x' - x <= 1 = True
        | otherwise = False
    isIncreasing _ = False

allOfSize :: Int -> [[Int]]
allOfSize 0 = [[]]
allOfSize x =
    let base = [[x'] | x' <- [1 .. x]]
        prefixes = [(x' :) | x' <- [1 .. x]]
        loop 0 zs = zs
        loop n zs = [f ys | f <- prefixes, ys <- loop (n -1) zs]
     in loop (x -1) base

isValidPartition :: [Int] -> [Int] -> Bool
isValidPartition xs p =
    let parts = uniq p
        couples = zip xs p
        groups = map (\x -> couples & filter ((== x) . snd) & map fst) parts
     in all ((<= 3) . sum) groups

allContiguousSublists :: [a] -> [SubList a]
allContiguousSublists [] = [(0, [], 0)]
allContiguousSublists [x] = [(1, [], 0), (0, [x], 0), (0, [], 1)]
allContiguousSublists xs = (0, xs, 0) : rest
  where
    hPref = map (sumPrefix 1) (allContiguousSublists $ tail xs)
    hSub = map (sumSuffix 1) (allContiguousSublists $ init xs)
    rest = hPref ++ hSub

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let parsed :: [Int] = map read inputLines
        devJolt = maximum parsed + 3
        chain = [0] ++ sort parsed ++ [devJolt]
        diff = differences chain
        ones = filter (== 1) diff
        threes = filter (== 3) diff
    putStrLn $ "Part1 result: " ++ show (length ones * length threes)
    -- let allChains = takeNext ([], chain)
    -- putStrLn $ "Part2 result: " ++ show (map (take 3 . fst) (take 10 allChains))
    let clusters = filter (\x -> x /= [] && x /= [3]) $ splitOn 3 diff
        res =
            clusters
                & map
                    ( \c ->
                        c
                            & allContiguousSublists
                            & filter (\(_, x, _) -> sum x <= 2)
                            & uniq
                            & length
                    )
                & sum
        allValidCombination cluster =
            let partitions = filter isValidMonotoneList $ allOfSize (length cluster)
             in length (filter (isValidPartition cluster) partitions)
        res2 = product (map allValidCombination clusters)
    putStrLn $ "Part2 result: " ++ show res2