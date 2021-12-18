module AOC.A21.ES14 where

import AOC.Utils
import Control.Lens
import Control.Monad.Reader
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Megaparsec
import Text.Megaparsec.Char

newtype InsRule a = InsRule (a, a, a)
    deriving (Show, Eq, Ord)

_triple :: Iso' (InsRule a) (a, a, a)
_triple = iso to' from'
  where
    from' p = InsRule p
    to' (InsRule p) = p

result :: InsRule a -> a
result (InsRule (_, _, z)) = z

_result :: Lens' (InsRule a) a
_result = lens result setter
  where
    setter (InsRule (x, y, _)) z = InsRule (x, y, z)

getRule :: Eq a => a -> a -> [InsRule a] -> Maybe (InsRule a)
getRule s e rules =
    rules
        ^? traverse
            . filteredBy (_triple . filtered (\(x, y, _) -> x == s && y == e))

type AppMonad = ReaderT [InsRule Char] IO

extendString :: String -> AppMonad String
extendString s = do
    allRules <- asks id
    let pairs = rollingWindows 2 s
        rules = pairs & map (\[x, y] -> result <$> getRule x y allRules)
        res = zip (map head pairs) rules ^.. traverse . beside id _Just
    pure $ res ++ [last s]

runSteps :: String -> Int -> AppMonad String
runSteps s n = do
    let loop s' n' = case n' of
            0 -> pure s'
            _ -> do
                s'' <- extendString s'
                s'' `seq` loop s'' (n' -1)
    loop s n

part1 :: String -> [InsRule Char] -> IO Integer
part1 = runPoly 10

part2 :: String -> [InsRule Char] -> IO Integer
part2 = error "Not working" -- runPoly 40

runPoly :: Int -> String -> [InsRule Char] -> IO Integer
runPoly n s rules = do
    res <- runReaderT (runSteps s n) rules
    putStrLn $ "Completed simulation, counting occurrencies"
    let occMap :: [(Char, Integer)] = M.toList $ countOccurrencies res
        mostCommon = fromJust $ maximumOf (traversed . _2) occMap
        leastCommon = fromJust $ minimumOf (traversed . _2) occMap
    -- putStrLn $ "After steps: " ++ res
    pure $ mostCommon - leastCommon

parseRule :: Parser (InsRule Char)
parseRule = do
    s <- alphaNumChar
    e <- alphaNumChar
    _ <- string " -> "
    r <- alphaNumChar <* eof
    pure $ InsRule (s, e, r)

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let s_start = head inputLines
    rules <- mapM (embedMaybe . parseMaybe parseRule) (drop 2 inputLines)
    res1 <- part1 s_start rules
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 s_start rules
    putStrLn $ "Res2: " ++ show res2