module AOC.A21.ES9 where

import AOC.Utils (Parser, embedMaybe, parseList)
import AOC.Utils.Grid
import Control.Lens
import Control.Monad
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

part1 :: Grid Int -> IO Int
part1 grid = do
    let lowPos = grid ^.. _positions . filtered (\(x, y) -> isLowPoint x y grid)
        -- TODO: use optics instead of mapMaybe
        -- lowPoints = grid ^.. filteredBy (_positions . filtered (\(x, y) -> isLowPoint x y grid)) . _points
        lowPoints = mapMaybe (\(x, y) -> getPosition x y grid) lowPos
    pure $ sumOf (traversed . to (+ 1)) lowPoints

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe lineParser) inputLines
    dataGrid <- createGrid parsed
    res1 <- part1 dataGrid
    putStrLn $ "Res1: " ++ show res1