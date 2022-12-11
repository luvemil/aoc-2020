module AOC.A22.ES11 where

import AOC.Utils
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let monkeysS = splitOn "" inputLines

    forM_ (zip monkeysS [0 ..]) $ \(ls, i) -> do
        putStrLn $ "=== Monkey " <> show i
        forM_ (tail ls) putStrLn

    putStrLn $ "A22E11 - part1: "
