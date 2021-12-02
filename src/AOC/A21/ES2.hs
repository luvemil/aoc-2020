module AOC.A21.ES2 where

import AOC.Utils
import Data.Foldable (Foldable (foldl'), foldlM, foldrM)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar, spaceChar, string)

data Direction = Up | Down | Forward
    deriving (Show, Eq)

data Step = Step Direction Int
    deriving (Show, Eq)

type Parser = Parsec Void String

directionParser :: Parser Direction
directionParser = do
    choice
        [ Up <$ string "up"
        , Forward <$ string "forward"
        , Down <$ string "down"
        ]

accumulate :: Step -> (Int, Int) -> (Int, Int)
accumulate (Step Forward x) (a, b) = (a + x, b)
accumulate (Step Up y) (a, b) = (a, b - y)
accumulate (Step Down y) (a, b) = (a, b + y)

stepParser :: Parser Step
stepParser = do
    direction <- directionParser
    _ <- many spaceChar
    amount <- read <$> many digitChar
    pure $ Step direction amount

accumulate2 :: Step -> (Int, Int, Int) -> (Int, Int, Int)
accumulate2 (Step Up y) (a, b, c) = (a, b, c - y)
accumulate2 (Step Down y) (a, b, c) = (a, b, c + y)
accumulate2 (Step Forward x) (a, b, c) = (a + x, b + (c * x), c)

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe stepParser) inputLines
    let (a, b) = foldl' (flip accumulate) (0, 0) parsed
    putStrLn $ "Res1: " ++ show (a * b)
    let (a', b', _) = foldl' (flip accumulate2) (0, 0, 0) parsed
    putStrLn $ "Res2: " ++ show (a' * b')

-- let accM step s = do
--         let res = accumulate2 step s
--         putStrLn $ show s ++ " -> (" ++ show step ++ ") -> " ++ show res
--         pure res
-- (a', b', c') <- foldlM (flip accM) (0, 0, 0) parsed
-- putStrLn $ "Res2: " ++ show (a', b', c')