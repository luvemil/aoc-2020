module AOC.A21.ES8 where

import AOC.Utils
import Control.Lens
import Control.Monad.State
import Data.Aeson (decode)
import qualified Data.Foldable as F
import Data.List (sort)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Text.Megaparsec (
    MonadParsec (eof, try),
    many,
    oneOf,
    parseMaybe,
 )
import Text.Megaparsec.Char

newtype Digit = Digit [Char]
    deriving (Show, Eq, Ord)

_string :: Iso' Digit String
_string = iso to' from'
  where
    to' (Digit x) = x
    from' = Digit

segmentParser :: Parser Char
segmentParser = oneOf [x | x <- "abcdefg"]

digitParser :: Parser Digit
digitParser = Digit . sort <$> many segmentParser

multiDigitParser :: Parser ([Digit], [Digit])
multiDigitParser = do
    iDigits <- many . try $ digitParser <* char ' '
    _ <- string "| "
    oDigits <- arrParser (char ' ') digitParser
    _ <- eof
    pure (iDigits, oDigits)

isUniq :: Digit -> Bool
isUniq (Digit ys)
    | length ys `elem` [2, 3, 4, 7] = True
    | otherwise = False

part1 :: [([Digit], [Digit])] -> IO Int
part1 xss = do
    let outputs = xss ^.. traversed . _2 . traversed . filtered isUniq
    pure $ length outputs

type DecypherState = ([Digit], M.Map Digit Int, M.Map Int Digit)

type AppMonad = StateT DecypherState IO

saveValue :: Digit -> Int -> AppMonad ()
saveValue digit val = do
    (ds, dict, revDict) <- get
    let ds' = filter (/= digit) ds
        dict' = M.insert digit val dict
        revDict' = M.insert val digit revDict
    put (ds', dict', revDict')
    pure ()

decodeByCardinality :: Int -> Int -> AppMonad ()
decodeByCardinality n val = do
    (ds, _, _) <- get
    let decoded = ds ^.. traversed . filtered (\(Digit x) -> length x == n)
        d = head decoded
    saveValue d val

isSubsetOf :: Digit -> Digit -> Bool
(Digit xs) `isSubsetOf` (Digit ys) = all (`elem` ys) xs

subsets :: Digit -> [Digit] -> [Digit]
subsets d = filter (`isSubsetOf` d)

decodeByCountingSubsets :: (Digit -> Bool) -> Int -> Int -> AppMonad ()
decodeByCountingSubsets f n val = do
    (ds, _, _) <- get
    fullList <- appGetFullList
    let myElems = ds ^.. traversed . filtered f
        decoded = myElems ^.. traversed . filtered (\x -> length (subsets x fullList) == n)
        d = head decoded
    -- liftIO . putStrLn $ "Filtered: " ++ show myElems
    saveValue d val

appGetFullList :: AppMonad [Digit]
appGetFullList = do
    (ds, _, m2) <- get
    pure $ ds ++ m2 ^.. traversed

appShowRemaining :: AppMonad ()
appShowRemaining = do
    (ds, _, _) <- get
    liftIO . putStrLn $ "Remaining: " ++ show ds

appShowFullList :: AppMonad ()
appShowFullList = appGetFullList >>= \x -> liftIO . putStrLn $ "Full List: " ++ show x

decodeFive :: AppMonad ()
decodeFive = do
    (_, _, m2) <- get
    six <- case M.lookup 6 m2 of
        Nothing -> error "Need 6 to decode 5"
        Just di -> pure di
    fullList <- appGetFullList
    let subs = subsets six fullList
    five <- case filter (/= six) subs ^? _head of
        Nothing -> error "5 not found"
        Just di -> pure di
    saveValue five 5

decodeUniques :: AppMonad ()
decodeUniques = do
    appShowRemaining
    decodeByCardinality 2 1
    decodeByCardinality 3 7
    decodeByCardinality 4 4
    decodeByCardinality 7 8

    decodeByCountingSubsets (\(Digit x) -> length x == 6) 3 0
    decodeByCountingSubsets (\(Digit x) -> length x == 6) 2 6
    decodeByCountingSubsets (\(Digit x) -> length x == 6) 6 9
    decodeByCountingSubsets (\(Digit x) -> length x == 5) 3 3
    decodeFive
    decodeByCardinality 5 2

execLine :: ([Digit], [Digit]) -> IO Int
execLine (dInput, dOutput) = do
    (_, fwMap, _) <- execStateT decodeUniques (dInput, M.empty, M.empty)
    res <- case forM dOutput (`M.lookup` fwMap) of
        Nothing -> error "Cannot decode line"
        Just ns -> pure ns
    let res' = F.foldl' (\acc cur -> acc * 10 + cur) 0 res
    pure res'

part2 :: [([Digit], [Digit])] -> IO Int
part2 ds = do
    xs <- forM ds execLine
    pure $ sum xs

main :: FilePath -> IO ()
main fp = do
    input <- lines <$> readFile fp
    digitLines <- parseList (embedMaybe . parseMaybe multiDigitParser) input
    res1 <- part1 digitLines
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 digitLines
    putStrLn $ "Res2: " ++ show res2