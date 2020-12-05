module AOC.Utils where

parseList :: (String -> IO a) -> [String] -> IO [a]
parseList parser input = do
    let loop (x : xs) = do
            y <- parser x
            ys <- loop xs
            pure $ y : ys
        loop [] = pure []
    loop input