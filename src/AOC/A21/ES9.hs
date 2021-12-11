module AOC.A21.ES9 where

import AOC.Utils (Parser, embedMaybe, parseList, uniq)
import AOC.Utils.Grid
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.List (sort)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)
import Text.Read (readMaybe)

lineParser :: Parser [Int]
lineParser = do
    chars :: [Char] <- many digitChar
    forM chars $ embedMaybe . readMaybe @Int . (: [])

isLowPoint :: Int -> Int -> Grid Int -> Bool
isLowPoint x y grid = fromMaybe False $ do
    val <- getPosition x y grid
    pure $ allOf (each . _Just) (val <) (getNeighbors x y grid)

part1 :: Grid Int -> IO (Int, [(Int, Int)])
part1 grid = do
    let lowPos = grid ^.. _positions . filtered (\(x, y) -> isLowPoint x y grid)
        -- TODO: use optics instead of mapMaybe
        -- lowPoints = grid ^.. filteredBy (_positions . filtered (\(x, y) -> isLowPoint x y grid)) . _points
        lowPoints = mapMaybe (\(x, y) -> getPosition x y grid) lowPos
    pure (sumOf (traversed . to (+ 1)) lowPoints, lowPos)

type BasinState = ([(Int, Int)], Grid Int)

type AppMonad = StateT BasinState IO

addToBasin :: [(Int, Int)] -> AppMonad [(Int, Int)]
addToBasin pos = do
    (oldPos, grid) <- get
    let newPos = uniq (oldPos ++ pos)
    put (newPos, grid)
    pure newPos

extendBasinFrom :: [(Int, Int)] -> AppMonad [(Int, Int)]
extendBasinFrom positions = do
    (_, grid) <- get
    let nbhs = positions ^.. traversed . to (\(x, y) -> getNeighborPos x y grid) . each . _Just
    newProcessed <- addToBasin positions
    let uNbhs = uniq nbhs ^.. traversed . filtered (`notElem` newProcessed)
    pure uNbhs

checkValidBasin :: Maybe Int -> Bool
checkValidBasin Nothing = False
checkValidBasin (Just x)
    | x >= 0 && x < 9 = True
    | otherwise = False

step :: [(Int, Int)] -> AppMonad [(Int, Int)]
step pos = do
    (_, grid) <- get
    let goodPos = pos ^.. traversed . filtered (\(x, y) -> checkValidBasin (getPosition x y grid))
    extendBasinFrom goodPos

fullBasin :: AppMonad [(Int, Int)]
fullBasin = do
    let loop xs = do
            step xs >>= \case
                [] -> pure ()
                xs' -> loop xs'
    (initXs, _) <- get
    loop initXs
    (res, _) <- get
    pure res

findBasin :: (Int, Int) -> Grid Int -> IO [(Int, Int)]
findBasin initPos grid = evalStateT fullBasin ([initPos], grid)

part2 :: [(Int, Int)] -> Grid Int -> IO Int
part2 lowPos grid = do
    basins <- forM lowPos $ \p -> evalStateT fullBasin ([p], grid)
    let results = take 3 . reverse . sort $ basins ^.. traversed . to length
    pure $ product results

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe lineParser) inputLines
    dataGrid <- createGrid parsed
    (res1, lowPos) <- part1 dataGrid
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 lowPos dataGrid
    putStrLn $ "Res2: " ++ show res2