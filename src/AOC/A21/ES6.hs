module AOC.A21.ES6 where

import AOC.Utils (Parser, arrParser, embedMaybe, intParser)
import Control.Lens.Operators
import Data.List (group)
import qualified Data.Map as M
import Data.Maybe
import Text.Megaparsec (eof, parseMaybe)
import Text.Megaparsec.Char

type LanternFishState = [Int]

stateParser :: Parser LanternFishState
stateParser = arrParser (char ',') intParser <* eof

step :: LanternFishState -> LanternFishState
step xs =
    let newBornSize = length (filter (== 0) xs)
        update :: Int -> Int
        update 0 = 6
        update x = x - 1
        updated = map update xs
        newBorns = case newBornSize of
            0 -> []
            x -> [8 | _ <- [1 .. x]]
     in updated ++ newBorns

runStep :: (a -> a) -> Int -> a -> a
runStep _ 0 s = s
runStep f n s
    | n < 0 = error "Only positive n"
    | otherwise = runStep f (n - 1) (f s)

type SecondLanternfishState = M.Map Int Integer

initSecondState :: [Int] -> SecondLanternfishState
initSecondState xs =
    let grouped = group xs
        mapped = [(head x, fromIntegral $ length x) | x <- grouped]
     in M.fromList mapped

step' :: SecondLanternfishState -> SecondLanternfishState
step' xs =
    let firstStep = M.mapKeys (\x -> x - 1) xs
        newBorns = fromMaybe 0 $ M.lookup (-1) firstStep
        updateNewBorns :: Integer -> Maybe Integer -> Maybe Integer
        updateNewBorns nb Nothing = Just nb
        updateNewBorns nb (Just x) = Just $ x + nb
        endStep =
            firstStep
                -- & M.adjust (+ newBorns) 6
                & M.alter (updateNewBorns newBorns) 6
                & M.delete (-1)
                & M.insert 8 newBorns
     in endStep

main :: FilePath -> Int -> IO ()
main fp days = do
    input <- readFile fp
    initialState <- embedMaybe . parseMaybe stateParser $ input
    let initialState' = initSecondState initialState
        res1 = runStep step days initialState
    putStrLn $ "Res1: " ++ show (length res1)