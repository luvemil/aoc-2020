{-# LANGUAGE TemplateHaskell #-}

module AOC.A21.ES16 where

import AOC.Utils (Parser, embedMaybe)
import Control.Lens (Fold, folding, ix, makePrisms, sumOf, (^?))
import Control.Monad (replicateM)
import qualified Data.Char as Char
import Data.Foldable (foldlM)
import Data.Maybe (fromJust)
import Text.Megaparsec (eof, many, oneOf, parse, parseMaybe, try)
import Text.Megaparsec.Char (char)

data Packet version typeId a
    = LitVal version typeId a
    | OpVal version typeId [Packet version typeId a]
    deriving (Show, Eq)

makePrisms ''Packet

_versions :: Fold (Packet v t a) v
_versions = folding getVersions
  where
    getVersions (LitVal v _ _) = [v]
    getVersions (OpVal v _ xs) = v : concatMap getVersions xs

type CPacket = Packet Int Int Int

hexCharToBin :: MonadFail m => Char -> m String
hexCharToBin x = case Char.toUpper x of
    '0' -> pure "0000"
    '1' -> pure "0001"
    '2' -> pure "0010"
    '3' -> pure "0011"
    '4' -> pure "0100"
    '5' -> pure "0101"
    '6' -> pure "0110"
    '7' -> pure "0111"
    '8' -> pure "1000"
    '9' -> pure "1001"
    'A' -> pure "1010"
    'B' -> pure "1011"
    'C' -> pure "1100"
    'D' -> pure "1101"
    'E' -> pure "1110"
    'F' -> pure "1111"
    _ -> fail "Not valid"

hexToBin :: MonadFail m => String -> m String
hexToBin s = concat <$> mapM hexCharToBin s

binCharToInt :: MonadFail m => Char -> m Int
binCharToInt '0' = pure 0
binCharToInt '1' = pure 1
binCharToInt _ = fail "Binary digit should be 0 or 1"

binStringToInt :: MonadFail m => [Char] -> m Int
binStringToInt [] = fail "Expecting at least 1 charater, found 0"
binStringToInt xs = foldlM (\acc cur -> (acc * 2 +) <$> binCharToInt cur) 0 xs

bitChar :: Parser Char
bitChar = oneOf ['0', '1']

parseLitBytes :: Char -> Parser String
parseLitBytes c = do
    h <- bitChar
    if h == c
        then replicateM 4 bitChar
        else fail $ "Expected leading " <> [c] <> ", found " <> [h]

parseLiteralValue :: Parser Int
parseLiteralValue = do
    vals <- many . try $ parseLitBytes '1'
    l <- parseLitBytes '0'
    binStringToInt $ concat (vals ++ [l])

parseOpPackets :: Parser [CPacket]
parseOpPackets = do
    lenTypeId <- bitChar
    case lenTypeId of
        '0' -> do
            -- total length
            l <- binStringToInt =<< replicateM 15 bitChar
            subInput <- replicateM l bitChar
            case parse (many parsePacket) "subPackets" subInput of
                Left _ -> fail "Cannot parse subpackets"
                Right pas -> pure pas
        '1' -> do
            -- num of subpackets
            n <- binStringToInt =<< replicateM 11 bitChar
            replicateM n parsePacket
        _ -> fail "Invalid length Type Id"

parsePacket :: Parser CPacket
parsePacket = do
    vI <- binStringToInt =<< replicateM 3 bitChar
    tI <- binStringToInt =<< replicateM 3 bitChar
    if tI /= 4
        then OpVal vI tI <$> parseOpPackets
        else LitVal vI tI <$> parseLiteralValue

parseFullPacket :: Parser CPacket
parseFullPacket = parsePacket <* many (char '0') <* eof

applyToSubpackets :: [a] -> (a -> a -> b) -> b
applyToSubpackets xs f = fromJust $ do
    x <- xs ^? ix 0
    y <- xs ^? ix 1
    pure $ f x y

runPacket :: CPacket -> Int
runPacket (LitVal _ _ x) = x
runPacket (OpVal _ t xs) =
    let subpks = map runPacket xs
     in case t of
            0 -> sum subpks
            1 -> product subpks
            2 -> minimum subpks
            3 -> maximum subpks
            5 -> applyToSubpackets subpks $ \x y -> if x > y then 1 else 0
            6 -> applyToSubpackets subpks $ \x y -> if x < y then 1 else 0
            7 -> applyToSubpackets subpks $ \x y -> if x == y then 1 else 0
            x -> error $ "Unexpected packet type id " <> show x

part1 :: CPacket -> IO Int
part1 packet = do
    let res = sumOf _versions packet
    pure res

part2 :: CPacket -> IO Int
part2 packet = pure $ runPacket packet

main :: FilePath -> IO ()
main fp = do
    input <- hexToBin =<< readFile fp
    packet <- embedMaybe . parseMaybe parseFullPacket $ input
    res1 <- part1 packet
    putStrLn $ "Res1: " <> show res1
    res2 <- part2 packet
    putStrLn $ "Res2: " <> show res2