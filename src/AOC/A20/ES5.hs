module AOC.A20.ES5 (main) where

import AOC.Utils
import Data.List (sort)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)

data BinaryDigit = One | Zero
    deriving (Show, Eq)

data Ticket = Ticket
    { row :: [BinaryDigit]
    , seat :: [BinaryDigit]
    }
    deriving (Show, Eq)

digitParser :: Char -> Char -> Parser BinaryDigit
digitParser zeroChar oneChar = do
    choice $
        [ Zero <$ char zeroChar
        , One <$ char oneChar
        ]

rowParser :: Parser [BinaryDigit]
rowParser = many $ digitParser 'F' 'B'

seatParser :: Parser [BinaryDigit]
seatParser = many $ digitParser 'L' 'R'

ticketParser :: Parser Ticket
ticketParser = do
    row <- rowParser
    seat <- seatParser
    if length row == 7 && length seat == 3
        then pure $ Ticket row seat
        else fail "Wrong length"

bToInt :: [BinaryDigit] -> Int
bToInt [] = 0
bToInt (Zero : xs) = bToInt xs
bToInt (One : xs) = 2 ^ length xs + bToInt xs

seatID :: Ticket -> Int
seatID (Ticket row seat) = bToInt row * 8 + bToInt seat

testStrings :: [String]
testStrings = ["BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"]

main :: FilePath -> IO ()
main fp = do
    testTickets <- parseList (embedMaybe . parseMaybe ticketParser) testStrings
    print $ map showAll testTickets
    inputLines <- lines <$> readFile fp
    tickets <- parseList (embedMaybe . parseMaybe ticketParser) inputLines
    let seatIDs = map seatID tickets
        maxID = maximum seatIDs
    putStrLn $ "Part1 result: " ++ show maxID
    let minID = minimum seatIDs
    let l = length seatIDs
        expected = fromIntegral (maxID + minID) * fromIntegral (l + 1) / 2
        real = sum seatIDs
        missing = expected - fromIntegral real
    putStrLn $ "Part 2: " ++ show missing
  where
    showAll t@(Ticket row seat) = (bToInt row, bToInt seat, seatID t)
