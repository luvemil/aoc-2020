module AOC.A21.ES3 where

import Control.Lens.Operators
import Data.List (foldl', maximumBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)

type OccurrencyMap a = M.Map a Int

modifyOrInsert :: Ord a => M.Map a b -> a -> b -> (b -> b) -> M.Map a b
modifyOrInsert m k defaultVal f =
    let curVal = M.lookup k m
        newVal = maybe defaultVal f curVal
     in M.insert k newVal m

updateOccurrencyMaps :: [OccurrencyMap Char] -> String -> [OccurrencyMap Char]
updateOccurrencyMaps = zipWith (\om c -> modifyOrInsert om c 1 (+ 1))

getMostFrequent :: Eq a => OccurrencyMap a -> Maybe a
getMostFrequent m
    | m == M.empty = Nothing
    | otherwise = Just res
  where
    (res, _) =
        M.toList m
            & maximumBy (\(_, x) (_, y) -> compare x y)

invertBit :: MonadFail m => Char -> m Char
invertBit '0' = pure '1'
invertBit '1' = pure '0'
invertBit _ = fail "Unexpected bit"

binaryCharToInt :: MonadFail m => Char -> m Int
binaryCharToInt '0' = pure 0
binaryCharToInt '1' = pure 1
binaryCharToInt _ = fail "Unexpected bit"

stringToBinaryInt :: MonadFail m => String -> Int -> m Int
stringToBinaryInt [] x = pure x
stringToBinaryInt (c : s) acc = do
    cur <- binaryCharToInt c
    stringToBinaryInt s $ acc * 2 + cur

getMostFreq2 :: OccurrencyMap Char -> Char
getMostFreq2 m =
    let count0 = fromMaybe 0 $ M.lookup '0' m
        count1 = fromMaybe 0 $ M.lookup '1' m
     in if count1 >= count0
            then '1'
            else '0'

getLeastFreq2 :: OccurrencyMap Char -> Char
getLeastFreq2 m =
    let count0 = fromMaybe 0 $ M.lookup '0' m
        count1 = fromMaybe 0 $ M.lookup '1' m
     in if count1 >= count0
            then '0'
            else '1'

step :: (OccurrencyMap Char -> Char) -> [(String, String)] -> [(String, String)]
step f vs =
    let curPos = map ((: []) . head . snd) vs
        occMap = head $ foldl' updateOccurrencyMaps [M.empty] curPos
        keep = f occMap
     in vs
            & filter ((== keep) . head . snd)
            & map (\(a, b) -> (a, tail b))

getRating :: MonadFail m => (OccurrencyMap Char -> Char) -> [String] -> m Int
getRating f d = do
    let dataPair = map (\x -> (x, x)) d
        loop f' d' = do
            let res = step f' d'
            case length res of
                0 -> fail "Not enough elements"
                1 -> do
                    let binaryString = fst . head $ res
                    stringToBinaryInt binaryString 0
                _ -> loop f' res
    loop f dataPair

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let initialOccMaps :: [OccurrencyMap Char] = head inputLines & map (const M.empty)
        resMaps = foldl' updateOccurrencyMaps initialOccMaps inputLines
        resMaybeBits = map getMostFrequent resMaps
        resBits = fromJust $ sequence resMaybeBits
    inverted <- mapM invertBit resBits
    gammaRate <- stringToBinaryInt resBits 0
    epsilonRate <- stringToBinaryInt inverted 0
    putStrLn $ "Res1: " ++ show (gammaRate * epsilonRate)
    -- Part 2
    oxyGenRating <- getRating getMostFreq2 inputLines
    co2ScrubRating <- getRating getLeastFreq2 inputLines
    putStrLn $ "OGR: " ++ show oxyGenRating
    putStrLn $ "CSR: " ++ show co2ScrubRating
    putStrLn $ "Res2: " ++ show (oxyGenRating * co2ScrubRating)