module AOC.A21.ES19 where

import AOC.Utils
import AOC.A21.ES19.Parsers
import Text.Megaparsec (parseMaybe)
import AOC.A21.ES19.Objects
import Control.Monad (forM, forM_)

parseGroup :: [String] -> IO [CBeacon]
parseGroup g = do
    parseList (embedMaybe . parseMaybe beaconParser)  (drop 1 g)

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  let groups = splitOn "" inputLines
  groupedBeacons <- forM groups parseGroup

  forM_ groupedBeacons $ \g -> do
    putStrLn "--- BEGIN GROUP ---"
    forM_ g print
    putStrLn "--- END GROUP ---"
  -- putStrLn "A21E19 - part1: "