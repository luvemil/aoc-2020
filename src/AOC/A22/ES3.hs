{-# LANGUAGE TemplateHaskell #-}

module AOC.A22.ES3 where

import AOC.Utils
import Control.Lens
import Data.Char (toLower, toUpper)
import Data.List (elemIndex, intersect)
import Data.Maybe (mapMaybe)
import Text.Megaparsec (MonadParsec (eof), many, parseMaybe)
import Text.Megaparsec.Char (alphaNumChar)

newtype Item = Item {unItem :: Char}
  deriving (Eq)
  deriving newtype (Show)

bases :: [Char]
bases = "abcdefghijklmnopqrstuvwxyz"

buildItem :: Char -> Maybe Item
buildItem c = elemIndex (toLower c) bases >> Just (Item c)

allItems :: [Item]
allItems = map Item bases <> map (Item . toUpper) bases

itemValue :: Item -> Maybe Int
itemValue x = elemIndex x allItems >>= \i -> Just $ i + 1

data Bag = Bag
  { leftBag :: ![Item],
    rightBag :: ![Item]
  }
  deriving (Show, Eq)

makeLenses ''Bag

bagTuple :: Iso' Bag ([Item], [Item])
bagTuple = iso fromBag toBag
  where
    fromBag :: Bag -> ([Item], [Item])
    fromBag (Bag x y) = (x, y)
    toBag :: ([Item], [Item]) -> Bag
    toBag (x, y) = Bag x y

-- Actions

itemsInBoth :: Bag -> [Item]
itemsInBoth bag =
  intersect (leftBag bag) (rightBag bag)
    & uniq

commonItems :: [Bag] -> [Item]
commonItems [] = []
commonItems [x] = uniq (x ^.. bagTuple . each . traversed)
commonItems (x : xs) = commonItems [x] `intersect` commonItems xs

-- Parsers

itemParser :: Parser Item
-- itemParser = embedMaybe . buildItem =<< digitChar
itemParser = do
  c <- alphaNumChar
  case buildItem c of
    Nothing -> fail $ "Found not valid char " <> [c]
    Just it -> pure it

bagParser :: Parser Bag
bagParser = do
  items <- many itemParser <* eof
  let l = length items
  if even l && l > 0
    then do
      let left = take (l `div` 2) items
          right = drop (l `div` 2) items
      pure $ Bag left right
    else fail $ "Found odd number of items: " <> show l

-- Parts

part1 :: [Bag] -> IO Int
part1 bags = do
  let items = concatMap itemsInBoth bags
      vals = mapMaybe itemValue items
  pure $ sum vals

part2 :: [Bag] -> IO Int
part2 bags = do
  let groups = chunks 3 bags
      common = map commonItems groups
  pure $ sumOf (traversed . traversed . to itemValue . _Just) common

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  bags <- parseList (embedMaybe . parseMaybe bagParser) inputLines

  res1 <- part1 bags
  putStrLn $ "A22E03 - part1: " <> show res1

  res2 <- part2 bags
  putStrLn $ "A22E03 - part2: " <> show res2