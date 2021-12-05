{-# LANGUAGE TupleSections #-}

module AOC.A21.ES4 where

import AOC.Utils (arrParser, embedMaybe, intParser, splitExact)
import Control.Lens
import Control.Monad (forM_, replicateM)
import qualified Data.Foldable as F
import Data.List (foldl', sort)
import qualified Data.Set as S
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, newline, spaceChar)
import Text.Read (readMaybe)
import Toml.Parser.Core (digitChar)

newtype BingoBoard a = BingoBoard [a]
    deriving (Show, Eq)

getRows :: BingoBoard a -> [[a]]
getRows (BingoBoard xs) = splitExact xs 5

getCols :: BingoBoard a -> [[a]]
getCols bb = foldl' myAcc initCols rows
  where
    rows = getRows bb
    initCols = head rows & map (const [])
    myAcc = zipWith (\col el -> col ++ [el])

bingoRows :: Fold (BingoBoard a) [a]
bingoRows = folding getRows

bingoCols :: Fold (BingoBoard a) [a]
bingoCols = folding getCols

isWinningRow :: (Foldable t, Ord a) => S.Set a -> t a -> Bool
isWinningRow ds row =
    let rowElems = S.fromList . F.toList $ row
     in rowElems `S.isSubsetOf` ds

winningLines ::
    (Foldable t, Ord a) =>
    [a] ->
    Fold (BingoBoard a) (t a) ->
    Fold (BingoBoard a) (t a)
winningLines ds theFold =
    let elemSet = S.fromList ds
     in theFold . filtered (isWinningRow elemSet)

isWinningBoard :: Ord a => [a] -> BingoBoard a -> Bool
isWinningBoard ds b =
    not
        ( null
            ( (b ^.. winningLines @[] ds bingoRows)
                ++ (b ^.. winningLines ds bingoCols)
            )
        )
getWinningBoards :: Ord a => [a] -> [BingoBoard a] -> [BingoBoard a]
getWinningBoards ds boards = boards ^.. traversed . filtered (isWinningBoard ds)

type GameResult a = (Maybe (BingoBoard a), [a])

accumulator1 :: Ord a => [BingoBoard a] -> GameResult a -> a -> GameResult a
accumulator1 _ x@(Just _, _) _ = x
accumulator1 boards (_, ds) d =
    let ds' = ds ++ [d]
        results = getWinningBoards ds' boards
     in case length results of
            0 -> (Nothing, ds')
            _ -> (Just (head results), ds')

accumulator2 :: (Show a, Ord a) => [BingoBoard a] -> GameResult a -> a -> IO (GameResult a)
accumulator2 _ x@(Just _, _) _ = pure x
accumulator2 boards (_, ds) d = do
    let ds' = ds ++ [d]
        results = getWinningBoards ds' boards
    case length results of
        0 -> do
            -- putStrLn $ "Got no winners with " ++ show (length ds') ++ " numbers extracted"
            pure (Nothing, ds')
        _ -> do
            -- putStrLn $ "Got some winners with " ++ show (length ds') ++ " numbers extracted"
            -- forM_ results printBoard
            pure (Just (head results), ds')

-- | ([(the board, the extractions it takes for it to win)], played nums, remaining boards)
type LosingGameResult a = ([(BingoBoard a, [a])], [a], [BingoBoard a])

accumulator3 :: (Ord a) => LosingGameResult a -> a -> IO (LosingGameResult a)
accumulator3 x@(_, _, []) _ = pure x
accumulator3 (oldBoards, ds, boards) d = do
    let ds' = ds ++ [d]
        winning = getWinningBoards ds' boards
        remaining = filter (not . (`elem` winning)) boards
    pure (oldBoards ++ map (,ds') winning, ds', remaining)

-- Parsing

type Parser = Parsec Void String

newtype Extraction = Extraction [Int]
    deriving (Show)

extractionParser :: Parser Extraction
extractionParser = Extraction <$> arrParser (char ',') intParser

boardParser :: Parser (BingoBoard Int)
boardParser = BingoBoard <$> replicateM 25 (many spaceChar *> intParser)

initialStateParser :: Parser (Extraction, [BingoBoard Int])
initialStateParser = do
    extraction <- extractionParser
    boards <- many . try $ some newline *> boardParser
    _ <- many newline <* eof
    pure (extraction, boards)

printBoard :: Show a => BingoBoard a -> IO ()
printBoard (BingoBoard xs) = do
    let nextLine = take 5 xs
    case nextLine of
        [] -> pure ()
        _ -> do
            print nextLine
            printBoard . BingoBoard . drop 5 $ xs

getBoardScore :: BingoBoard Int -> [Int] -> Int
getBoardScore (BingoBoard bbElems) numbers =
    let unmarked = S.difference (S.fromList bbElems) (S.fromList numbers)
        unmarkedSum = sum unmarked
     in unmarkedSum * last numbers

main :: FilePath -> IO ()
main fp = do
    input <- readFile fp

    -- parseTest initialStateParser input

    -- error "Done"
    (Extraction extracted, boards) <- case parse initialStateParser "" input of
        Left peb -> do
            putStrLn "Error in parsing: "
            putStrLn (errorBundlePretty peb)
            error "failure"
        Right x -> pure x
    putStrLn $ "Success in parsing, n. of boards: " ++ show (length boards) --, got results: " ++ show (extracted, boards)

    -- -- (Extraction extracted, boards) <- embedMaybe . parseMaybe initialStateParser $ input
    -- let (maybeBoard, numbers) = foldl' (accumulator1 boards) (Nothing, []) extracted
    (maybeBoard, numbers) <- F.foldlM (accumulator2 boards) (Nothing, []) extracted
    putStrLn "And the winner is"
    board <- case maybeBoard of
        Nothing -> error "No winner found"
        Just bb -> pure bb
    -- printBoard  board
    let (BingoBoard bElems) = board
    putStrLn $ "Board elems: " ++ show (sort bElems)
    putStrLn $ "Extracted: " ++ show (sort numbers)
    -- let (BingoBoard bbElems) = board
    --     unmarked = S.difference (S.fromList bbElems) (S.fromList numbers)
    --     unmarkedSum = sum unmarked
    --     res1 = unmarkedSum * last numbers
    -- putStrLn $ "Unmarked: " ++ show unmarked
    -- putStrLn $ "Unmarked sum: " ++ show unmarkedSum
    putStrLn $ "Res1: " ++ show (getBoardScore board numbers)

    (p2Boards, _, _) <- F.foldlM accumulator3 ([], [], boards) extracted
    let (lastBoard, lastNumbers) = last p2Boards
    putStrLn $ "Res2: " ++ show (getBoardScore lastBoard lastNumbers)

-- putStrLn $ "Got boards: " ++ show (length p2Boards)
-- print p2Boards
-- putStrLn $ "ds: " ++ show ds
-- putStrLn $ "remaining length: " ++ show (length remaining)

-- Let's manually check if any other board is winning
-- let (rows, cols) = (boards ^.. traversed . bingoRows, boards ^.. traversed . bingoCols)
--     myData = zip3 boards rows cols
-- putStrLn $ show myData