{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module AOC.A22.ES7 where

import AOC.Utils
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, fromJust)
import System.FilePath
import System.FilePath.Lens
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

data Com = ComCD !String | ComLS
  deriving (Show, Eq)

data Item = ItemDir !String | ItemFile !Integer !String
  deriving (Show, Eq)

makePrisms ''Item

type MyState =
  ( String, -- current path
    M.Map String [Item] -- fs
  )

type Comp = StateT MyState Identity

prettyPrint :: Either Com Item -> String
prettyPrint (Left (ComCD a)) = "$ cd " <> a
prettyPrint (Left ComLS) = "$ ls"
prettyPrint (Right (ItemDir a)) = "dir " <> a
prettyPrint (Right (ItemFile size a)) = show size <> " " <> a

runStep :: Either Com Item -> Comp ()
runStep (Left (ComCD "/")) = modify' $ \s -> s & _1 .~ "/"
runStep (Left (ComCD "..")) = modify' $ \s -> s & _1 %~ view directory
runStep (Left (ComCD a)) = modify' $ \s ->
  s
    & _1 </>~ a
    & id %~ \(curPath, fs) ->
      ( curPath,
        M.insertWith (\_ old_val -> old_val) curPath [] fs
      )
runStep (Left ComLS) = pure ()
runStep (Right item) = modify' $ \(curPath, fs) -> (curPath, M.update (\x -> Just $ x <> [item]) curPath fs)

type DuState = StateT (M.Map FilePath Integer) (ReaderT (M.Map FilePath [Item]) IO)

duRecursive :: FilePath -> DuState Integer
duRecursive fp = do
  sizeM <- gets $ M.lookup fp
  case sizeM of
    Nothing -> do
      -- liftIO . putStrLn $ "dir " <> fp <> " not found in cache, entering"
      items <- asks (fromMaybe [] . M.lookup fp)
      let itemSizes = sumOf (traversed . _ItemFile . _1) items
      dirSizes :: [Integer] <- forM (toListOf (traversed . _ItemDir) items) $ \s -> duRecursive (fp </> s)
      let totalSize = itemSizes + sum dirSizes
      modify' $ M.insert fp totalSize
      pure totalSize
    Just n -> pure n

du :: FilePath -> M.Map FilePath [Item] -> IO (M.Map FilePath Integer)
du fp fs =
  duRecursive fp
    & flip execStateT M.empty
    & flip runReaderT fs

-- Parsers

comParser :: Parser Com
comParser = do
  _ <- string "$ "
  choice
    [ ComLS <$ string "ls",
      ComCD
        <$> ( string "cd "
                *> ( try (string "/")
                       <|> try (string "..")
                       <|> some alphaNumChar
                   )
            )
    ]

itemParser :: Parser Item
itemParser =
  choice
    [ ItemDir <$> (string "dir " *> some alphaNumChar),
      do
        size <- embedMaybe . readMaybe =<< (some digitChar <* char ' ')
        name <- some alphaNumChar
        pure $ ItemFile size name
    ]

lineParser :: Parser (Either Com Item)
lineParser =
  choice
    [ Left <$> comParser,
      Right <$> itemParser
    ]

-- Parts

part1 :: M.Map FilePath [Item] -> IO (M.Map FilePath Integer, Integer)
part1 fs = do
  sizes <- du "/" fs
  -- print sizes
  pure . (sizes,) $ sumOf (traversed . filtered (< 100000)) sizes

part2 :: Integer -> Integer -> M.Map FilePath Integer -> IO Integer
part2 totDskSize reqDskSize sizes = do
  let usdDskSize = sizes M.! "/"
      spaceToFree = reqDskSize + usdDskSize - totDskSize
      toDelete = fromJust $ minimumOf (traversed . filtered (>= spaceToFree)) sizes
  pure toDelete

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp

  rows <- forM inputLines $ \il -> do
    case parse lineParser "Line" il of
      Left peb -> fail $ errorBundlePretty peb
      Right e -> pure e

  let final = snd $ execState (forM rows runStep) ("", M.singleton "/" [])

  (s, res1) <- part1 final
  putStrLn $ "A22E7 - part1: " <> show res1

  res2 <- part2 70000000 30000000 s
  putStrLn $ "A22E7 - part2: " <> show res2