module AOC.A20.ES7 (main) where

import AOC.Utils
import Data.Function
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type BagID = String

data BagRule = BagRule
    { color :: BagID
    , contents :: [(Int, BagID)]
    }
    deriving (Eq, Show)

-- | TODO: improve this implementation
parseColor :: Parser BagID
parseColor = do
    w <- many letterChar
    case w of
        "bag" -> pure ""
        "bags" -> pure ""
        _ -> do
            space
            y <- parseColor
            case y of
                "" -> pure w
                _ -> pure $ w ++ " " ++ y

parseContent :: Parser (Int, BagID)
parseContent = do
    amount <- many digitChar
    space
    bID <- parseColor
    pure (read amount, bID)

parseRule :: Parser BagRule
parseRule = do
    bID <- parseColor
    _ <- space >> string "contain"
    nextWord <- space *> lookAhead (many alphaNumChar)
    case nextWord of
        "no" -> do
            _ <- many printChar -- Consuming the rest of the string
            pure $ BagRule bID []
        _ -> do
            contents <- sepBy (space *> parseContent) (char ',')
            _ <- char '.' -- Consuming the final period
            pure $ BagRule bID contents

testStr :: String
testStr = "light red bags contain 1 bright white bag, 2 muted yellow bags."

bagContains :: [BagRule] -> BagID -> [BagID]
bagContains allRules bID =
    allRules
        & filter (\(BagRule _ cs) -> bID `elem` map snd cs)
        & map color

cumulativeContains :: [BagRule] -> [BagID] -> [BagID]
cumulativeContains allRules bIDs = Set.toList (oldIDs `Set.union` newIDs)
  where
    oldIDs = Set.fromList bIDs
    newIDs = Set.fromList $ bIDs >>= bagContains allRules

getBagsInside :: [BagRule] -> BagID -> [(Int, BagID)]
getBagsInside allRules bID =
    allRules >>= \case
        BagRule b cs | b == bID -> cs
        _ -> []

countRecursiveContents :: [BagRule] -> BagID -> Int
countRecursiveContents allRules bID =
    let cs = getBagsInside allRules bID
     in case cs of
            [] -> 0
            _ ->
                let thisSize = sum (map fst cs)
                    rest = cs >>= \(qt, bag) -> pure $ qt * countRecursiveContents allRules bag
                 in thisSize + sum rest

-- testStr = "dotted black bags contain no other bags."

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe parseRule) inputLines
    let target = "shiny gold" :: BagID
        fun1 (x, y) = (x, cumulativeContains x y)
        fixed = snd $ findFixedPointIter (==) fun1 (parsed, [target])
        correct = filter (/= target) fixed
    putStrLn $ "Part1 result: " ++ show (length correct)
    putStrLn $ "Part2 result: " ++ show (countRecursiveContents parsed target)