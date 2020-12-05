module AOC.Utils where

import Data.List (foldl')

uniq :: Eq a => [a] -> [a]
uniq = foldl' add []
  where
    add as a = if a `elem` as then as else a : as

parseList :: (String -> IO a) -> [String] -> IO [a]
parseList parser input = do
    let loop (x : xs) = do
            y <- parser x
            ys <- loop xs
            pure $ y : ys
        loop [] = pure []
    loop input