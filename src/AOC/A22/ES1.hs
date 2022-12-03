module AOC.A22.ES1 (main) where

import AOC.Utils (Parser, embedMaybe, parseList, splitOn)
import Control.Applicative
import Data.Foldable (foldl')
import Data.List (sortOn)
import Text.Megaparsec (parseMaybe)
import Text.Megaparsec.Char (digitChar)
import Text.Read (readMaybe)

lineParser :: Parser Int
lineParser = embedMaybe . readMaybe =<< many digitChar

data Carrier = Carrier [Int]
  deriving (Show, Eq)

getCarriers :: [String] -> [Maybe Carrier]
getCarriers ls =
  let lss = splitOn "" ls
   in map
        ( \l -> do
            r <- parseList (parseMaybe lineParser) l
            pure $ Carrier r
        )
        lss

w :: Carrier -> Int
w (Carrier xs) = sum xs

biggest :: [Carrier] -> Carrier
biggest = foldl' (\acc cur -> if w cur > w acc then cur else acc) (Carrier [0])

part1 :: [Carrier] -> IO Int
part1 cs = do
  let (Carrier r) = biggest cs
      s = sum r
  pure s

part2 :: [Carrier] -> IO Int
part2 cs = do
  let cSorted = sortOn (\x -> - w x) cs
      top3 = take 3 cSorted
  pure . sum $ map w top3

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  carriers <- embedMaybe . sequenceA $ getCarriers inputLines

  res1 <- part1 carriers

  res2 <- part2 carriers

  putStrLn $ "A21E1 - part1: " <> show res1

  putStrLn $ "A21E1 - part2: " <> show res2