module AOC.A21.ES13 where

import AOC.Utils
import AOC.Utils.Grid
import Control.Lens
import qualified Data.Foldable as F
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char (char, digitChar, string)

data SFold = FoldH Int | FoldV Int
    deriving (Show, Eq)

type Point = (Int, Int)

positionParser :: Parser Point
positionParser = do
    x <- intParser <* char ','
    y <- intParser <* eof
    pure (x, y)

foldParser :: Parser SFold
foldParser = do
    _ <- string "fold along "
    dir <- oneOf ['x', 'y'] <* char '='
    l <- intParser <* eof
    case dir of
        'x' -> pure $ FoldH l
        'y' -> pure $ FoldV l
        _ -> fail "dir should be x or y"

applyFold :: SFold -> Point -> Point
applyFold (FoldH x') (x, y) =
    if x <= x'
        then (x, y)
        else (2 * x' - x, y)
applyFold (FoldV y') (x, y) =
    if y <= y'
        then (x, y)
        else (x, 2 * y' - y)

step :: [Point] -> SFold -> [Point]
step pts f = map (applyFold f) pts

part1 :: [Point] -> [SFold] -> IO Int
part1 pts folds = do
    let res = step pts (head folds)
        upts = uniq res
    pure $ length upts

ptsToGrid :: MonadFail m => [Point] -> m (Grid Char)
ptsToGrid pts = do
    let w = fromMaybe 0 $ maximumOf (traversed . _1) pts
        h = fromMaybe 0 $ maximumOf (traversed . _2) pts
        xs = [['.' | _ <- [0 .. w]] | _ <- [0 .. h]]
    grid <- createGrid xs
    pure $ grid & itraversed %@~ \i _ -> if i `elem` pts then '#' else '.'

part2 :: [Point] -> [SFold] -> IO String
part2 pts folds = do
    let res = F.foldl' step pts folds
        upts = uniq res
    grid <- ptsToGrid upts
    pure $ prettyGrid grid

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let splitted = splitOn "" inputLines
    (positionsList, foldsList) <-
        if length splitted == 2
            then pure $ ((), ()) & unsafePartsOf each .~ splitted
            else error "Wrong size"
    positions <- mapM (embedMaybe . parseMaybe positionParser) positionsList
    folds <- mapM (embedMaybe . parseMaybe foldParser) foldsList
    res1 <- part1 positions folds
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 positions folds
    putStrLn $ "Res2: \n" ++ res2