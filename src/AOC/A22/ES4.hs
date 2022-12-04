module AOC.A22.ES4 where

import AOC.Utils
import Control.Lens
import Control.Monad (forM)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

newtype Segment a = Segment {unSegment :: (a, a)}
  deriving (Eq)
  deriving newtype (Show)

makeSegment :: (Ord a, Show a) => a -> a -> Either String (Segment a)
makeSegment x y =
  if x <= y
    then Right $ Segment (x, y)
    else Left $ "Error: lower bound greater than upper bound: " <> show (x, y)

segContains :: Ord a => Segment a -> Segment a -> Bool
segContains (Segment (x, y)) (Segment (a, b)) = all (\t -> t >= x && t <= y) [a, b]

segDisjoint :: Ord a => Segment a -> Segment a -> Bool
segDisjoint (Segment (x, y)) (Segment (a, b)) = a > y || b < x

-- Parsers

parseSegment :: Parser (Segment Int)
parseSegment = do
  l <- embedMaybe . readMaybe =<< many digitChar <* char '-'
  r <- embedMaybe . readMaybe =<< many digitChar
  case makeSegment l r of
    Left s -> fail s
    Right seg -> pure seg

parseCouple :: Parser (Segment Int, Segment Int)
parseCouple = do
  l <- parseSegment <* char ','
  r <- parseSegment
  pure (l, r)

-- Parts

part1 :: [(Segment Int, Segment Int)] -> IO Int
part1 couples = do
    let res = filter (\(a, b) -> a `segContains` b || b `segContains` a) couples
    pure $ length res


part2 :: [(Segment Int, Segment Int)] -> IO Int
part2 couples = do
    let res = filter (\(a, b) -> not (a `segDisjoint` b)) couples
    pure $ length res

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  couples <- forM inputLines $ embedMaybe . parseMaybe parseCouple

  res1 <- part1 couples
  putStrLn $ "A22E4 - part1: " <> show res1

  res2 <- part2 couples
  putStrLn $ "A22E4 - part2: " <> show res2
