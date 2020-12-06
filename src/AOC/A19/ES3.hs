module AOC.A19.ES3 (main) where

import AOC.Utils
import qualified Data.Set as Set
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char)
import Toml.Parser.Core (digitChar)

type Parser = Parsec Void String

data Direction = DU | DD | DR | DL
    deriving (Show, Eq)

data Segment = Segment
    { direction :: Direction
    , amount :: Int
    }
    deriving (Show, Eq)

newtype Line = Line [Segment]
    deriving (Show, Eq)

directionParser :: Parser Direction
directionParser = do
    choice
        [ DU <$ char 'U'
        , DD <$ char 'D'
        , DR <$ char 'R'
        , DL <$ char 'L'
        ]

segmentParser :: Parser Segment
segmentParser = do
    direction <- directionParser
    amountStr <- many digitChar
    let amount = read amountStr
    pure $ Segment direction amount

lineParser :: Parser Line
lineParser = do
    x <- segmentParser
    xs <- many (char ',' >> segmentParser)
    pure $ Line (x : xs)

type Point = (Int, Int)

toVector :: Direction -> Point
toVector DU = (0, 1)
toVector DD = (0, -1)
toVector DR = (1, 0)
toVector DL = (-1, 0)

segmentTrace :: Point -> Segment -> [Point]
segmentTrace (x0, y0) (Segment d a) = [(x0 + x1 * i, y0 + y1 * i) | i <- [1 .. a]]
  where
    (x1, y1) = toVector d

lineTrace :: Point -> Line -> [Point]
lineTrace _ (Line []) = []
lineTrace p (Line (x : xs)) =
    let t = segmentTrace p x
        p' = last t
     in t ++ lineTrace p' (Line xs)

distance :: Point -> Point -> Int
distance (x, y) (x', y') = abs (x - x') + abs (y - y')

distanceTo :: Point -> [Point] -> Int
distanceTo p = (+ 1) . length . takeWhile (/= p)

getIntersections :: [Point] -> [Point] -> [Point]
getIntersections t1 t2 =
    let s1 = Set.fromList t1
        s2 = Set.fromList t2
        si = Set.intersection s1 s2
        intersections = Set.toList si
     in intersections

ti1 :: String
ti1 = "R75,D30,R83,U83,L12,D49,R71,U7,L72,U62,R66,U55,R34,D71,R55,D58,R83"

ti2 :: String
ti2 = "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51,U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe lineParser) inputLines
    let [t1, t2] = map (lineTrace (0, 0)) parsed
        intersections = getIntersections t1 t2
        res1 = minimum (map (distance (0, 0)) intersections)
    putStrLn $ "Part1 Result: " ++ show res1
    -- test
    tparsed <- parseList (embedMaybe . parseMaybe lineParser) [ti1, ti2]
    let its2 = getIntersections t1 t2
        res2 = minimum [distanceTo p t1 + distanceTo p t2 | p <- its2]
    putStrLn $ "Part2 Result: " ++ show res2