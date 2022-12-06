{-# LANGUAGE TemplateHaskell #-}
module AOC.A22.ES5 where

import AOC.Utils
import Control.Lens
import Control.Monad (forM, forM_, replicateM_)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes, fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, char, digitChar, string)
import Text.Read (readMaybe)
import Data.Foldable (foldl')
import Data.List (sortBy)
import Data.Ord (comparing)

newtype Stack a = Stack {unStack :: [a]}
  deriving (Eq)
  deriving newtype (Show)

makePrisms ''Stack

buildStacks :: [(Char, [Maybe Char])] -> M.Map Char (Stack Char)
buildStacks listed =
  let p = listed & traversed . _2 %~ Stack . catMaybes
   in M.fromList p

popStack :: Int -> Stack Char -> (Stack Char, [Char])
popStack n (Stack cs) = (Stack (drop n cs), take n cs)

pushStack :: [Char] -> Stack Char -> Stack Char
pushStack cs (Stack cs') = Stack (cs <> cs')

-- Parsers

parseStackItem :: Parser Char
parseStackItem = do
  char '[' *> alphaNumChar <* char ']'

parseItemSlot :: Parser (Maybe Char)
parseItemSlot = do
  item <- observing $ try parseStackItem
  case item of
    Left _ -> do
      -- consume 3 spaces
      replicateM_ 3 $ char ' '
      pure Nothing
    Right c -> pure $ Just c

parseItemsRow :: Parser [Maybe Char]
parseItemsRow = do
  some (parseItemSlot <* (try (char ' ' >> pure ()) <|> eof))

parseLabelsRow :: Parser [Char]
parseLabelsRow = do
  some (char ' ' *> alphaNumChar <* char ' ' <* (try (char ' ' >> pure ()) <|> eof))

parseAction :: Parser (Int, Char, Char)
parseAction = do
  _ <- string "move "
  qty :: Int <- embedMaybe . readMaybe =<< many digitChar <* char ' '
  _ <- string "from "
  startLabel :: Char <- alphaNumChar <* char ' '
  _ <- string "to "
  endLabel :: Char <- alphaNumChar
  pure (qty, startLabel, endLabel)

-- Actions

evalAction :: M.Map Char (Stack Char) -> (Int, Char, Char) -> M.Map Char (Stack Char)
evalAction s (qty, startLabel, endLabel) =
    -- s & itraversed  %@~ \i x -> x
    let (startStack, vals) = fromJust $ popStack qty <$> s ^? ix startLabel
     in s & ix startLabel .~ startStack
          & ix endLabel %~ pushStack (reverse vals)

evalAction2 :: M.Map Char (Stack Char) -> (Int, Char, Char) -> M.Map Char (Stack Char)
evalAction2 s (qty, startLabel, endLabel) =
    -- s & itraversed  %@~ \i x -> x
    let (startStack, vals) = fromJust $ popStack qty <$> s ^? ix startLabel
     in s & ix startLabel .~ startStack
          & ix endLabel %~ pushStack vals

-- Parts

part1 :: M.Map Char (Stack Char) -> [(Int, Char, Char)] -> IO String
part1 stacks actions = do
    let finalStacks = foldl' evalAction stacks actions
        res' = sortBy (comparing fst) $ finalStacks ^@.. itraversed
        res = res' ^.. traversed . _2 . _Stack . to head
    pure res


part2 :: M.Map Char (Stack Char) -> [(Int, Char, Char)] -> IO String
part2 stacks actions = do
    let finalStacks = foldl' evalAction2 stacks actions
        res' = sortBy (comparing fst) $ finalStacks ^@.. itraversed
        res = res' ^.. traversed . _2 . _Stack . to head
    pure res

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  let splitted = splitOn "" inputLines
      stacksLines = head splitted
      actionsS = head $ tail splitted
      stacksS = init stacksLines
      labelsS = last stacksLines
  stackItems <- forM stacksS $ \sl ->
    case parse parseItemsRow "Items Row" sl of
      Left peb -> fail $ errorBundlePretty peb
      Right x -> pure x
  labels <- case parse parseLabelsRow "Labels Row" labelsS of
    Left peb -> fail $ errorBundlePretty peb
    Right x -> pure x
  actions <- forM actionsS $ \as ->
    case parse parseAction "Action" as of
      Left peb -> fail $ errorBundlePretty peb
      Right x -> pure x

  let stacks = buildStacks $ zip labels (L.transpose stackItems)

  res1 <- part1 stacks actions
  putStrLn $ "AOC - part1: " <> res1

  res2 <- part2 stacks actions
  putStrLn $ "AOC - part2: " <> res2