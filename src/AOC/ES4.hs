module AOC.ES4 (main) where

import AOC.Utils

type Passport = [(String, String)]

psptRequiredKeys :: [String]
psptRequiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

isValid1 :: Passport -> Bool
isValid1 pspt =
    let keys = map fst pspt
        values = map snd pspt
     in all (`elem` keys) psptRequiredKeys && notElem "" values

parsePassport :: [String] -> IO Passport
parsePassport [] = pure []
parsePassport (x : xs) = do
    let entries = splitOn ' ' x
        parseEntry s = do
            let entry = splitOn ':' s
            case entry of
                [key, value] -> pure (key, value)
                _ -> fail $ "Cannot parse entry: " ++ s
    parsedLine <- parseList parseEntry entries
    rest <- parsePassport xs
    pure $ parsedLine ++ rest

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let splitData = splitOn "" inputLines
    passports <- parseList parsePassport splitData
    let validPassports = filter isValid1 passports
    putStrLn $ "Part1 Result: " ++ show (length validPassports)