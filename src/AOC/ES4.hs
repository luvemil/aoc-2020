module AOC.ES4 (main) where

import AOC.Utils
import qualified Data.Map as M
import Text.Read (readMaybe)
import Text.Regex.TDFA

type Passport = M.Map String String

psptRequiredKeys :: [String]
psptRequiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

type Match = (String, String, String, [String])

parseRegexReader :: MonadFail m => String -> String -> (String -> m a) -> m a
parseRegexReader val regex parser = do
    (_, m, _, _) :: Match <- val =~~ regex
    if m /= ""
        then parser m
        else fail "Cannot parse"

parseRegexString :: String -> String -> Maybe String
parseRegexString regex val = parseRegexReader val regex $ \x -> pure x

parseRegexInt :: String -> String -> Maybe Int
parseRegexInt regex val = parseRegexReader val regex readMaybe

mkValidator :: (String -> Maybe a) -> (a -> Bool) -> String -> Bool
mkValidator parser validator x = maybe False validator (parser x)

mkSimpleValidator :: (String -> Maybe a) -> String -> Bool
mkSimpleValidator parser = mkValidator parser $ const True

parseFourDigits :: String -> Maybe Int
parseFourDigits = parseRegexInt "^[0-9][0-9][0-9][0-9]$"

psptValidationExpr :: [(String, String -> Bool)]
psptValidationExpr =
    [
        ( "byr"
        , let validator x = x >= 1920 && x <= 2002
              parser = parseFourDigits
           in mkValidator parser validator
        )
    ,
        ( "iyr"
        , let validator x = x >= 2010 && x <= 2020
              parser = parseFourDigits
           in mkValidator parser validator
        )
    ,
        ( "eyr"
        , let validator x = x >= 2020 && x <= 2030
              parser = parseFourDigits
           in mkValidator parser validator
        )
    ,
        ( "hgt"
        , let validCm x = x >= 150 && x <= 193
              validIn x = x >= 59 && x <= 76
              parser val = do
                (_, _, _, [h, u]) :: Match <- val =~~ ("^([0-9]+)(cm|in)$" :: String)
                height :: Int <- readMaybe h
                case u of
                    "cm" -> if validCm height then pure height else Nothing
                    "in" -> if validIn height then pure height else Nothing
                    _ -> Nothing
           in mkSimpleValidator parser
        )
    ,
        ( "hcl"
        , let parser = parseRegexString "^#[a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9][a-z0-9]$"
           in mkSimpleValidator parser
        )
    ,
        ( "ecl"
        , let parser = parseRegexString "^(amb|blu|brn|gry|grn|hzl|oth)$"
           in mkSimpleValidator parser
        )
    ,
        ( "pid"
        , let parser = parseRegexInt "^[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]$"
           in mkSimpleValidator parser
        )
    ]

isValid1 :: Passport -> Bool
isValid1 pspt =
    let keys = map fst (M.toList pspt)
        values = map snd (M.toList pspt)
     in all (`elem` keys) psptRequiredKeys && notElem "" values

validateKey :: (String, String -> Bool) -> Passport -> Bool
validateKey (key, validator) pspt =
    let value = M.lookup key pspt
     in maybe False validator value

isValid2 :: Passport -> Bool
isValid2 pspt =
    let keys = map fst (M.toList pspt)
     in all (`elem` keys) psptRequiredKeys && all (`validateKey` pspt) psptValidationExpr

parsePassport :: [String] -> IO Passport
parsePassport [] = pure M.empty
parsePassport (x : xs) = do
    let entries = splitOn ' ' x
        parseEntry s = do
            let entry = splitOn ':' s
            case entry of
                [key, value] -> pure (key, value)
                _ -> fail $ "Cannot parse entry: " ++ s
    parsedLine <- parseList parseEntry entries
    rest <- parsePassport xs
    pure $ M.union (M.fromList parsedLine) rest

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    let splitData = splitOn "" inputLines
    passports <- parseList parsePassport splitData
    let validPassports = filter isValid1 passports
    putStrLn $ "Part1 Result: " ++ show (length validPassports)
    let valid2 = filter isValid2 passports
    putStrLn $ "Part2 Result: " ++ show (length valid2)