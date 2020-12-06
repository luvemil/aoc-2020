module AOC.ES6 (main) where

import AOC.Utils (splitOn, uniq)
import Data.Set hiding (map)

groupAnswers :: [String] -> String
groupAnswers = uniq . concat

newtype Answers a = Answers (Set a)

type PAns = Answers Char

getPersons :: [String] -> [PAns]
getPersons = map (Answers . fromList)

groupAnswers2 :: [PAns] -> PAns
groupAnswers2 = foldr1 intersectAnswers
  where
    intersectAnswers (Answers s1) (Answers s2) = Answers $ intersection s1 s2

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let groups = splitOn "" inputLines
        answers = map groupAnswers groups
        counts = map length answers
    putStrLn $ "Part 1: " ++ show (sum counts)
    let answers2 = map (groupAnswers2 . getPersons) groups
        counts2 = map (\(Answers t) -> size t) answers2
    putStrLn $ "Part 2: " ++ show (sum counts2)