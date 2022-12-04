module AOC.A20.ES6 (main) where

import AOC.Utils (splitOn, uniq)
import Data.Set hiding (map)

groupAnswers :: [String] -> String
groupAnswers = uniq . concat

type PAns = Set Char

getPersons :: [String] -> [PAns]
getPersons = map fromList

groupAnswers2 :: [PAns] -> PAns
groupAnswers2 = foldr1 intersection

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let groups = splitOn "" inputLines
        answers = map groupAnswers groups
        counts = map length answers
    putStrLn $ "Part 1: " ++ show (sum counts)
    let answers2 = map (groupAnswers2 . getPersons) groups
        counts2 = map size answers2
    putStrLn $ "Part 2: " ++ show (sum counts2)