module AOC.ES1 (main) where

main :: FilePath -> IO ()
main input = do
    parsed <- lines <$> readFile input
    putStrLn $ "ES1: " ++ show parsed
