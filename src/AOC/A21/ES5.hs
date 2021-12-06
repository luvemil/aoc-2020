module AOC.A21.ES5 where

import AOC.Utils (Parser, embedMaybe, intParser)
import Control.Lens
import Control.Monad (forM_)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

data Point = Point Int Int
    deriving (Show, Eq, Ord)

data Segment = Segment Point Point
    deriving (Show, Eq)

-- Lenses

_x :: Lens' Point Int
_x = lens getter setter
  where
    getter (Point x _) = x
    setter (Point _ y) x' = Point x' y

_y :: Lens' Point Int
_y = lens getter setter
  where
    getter (Point _ y) = y
    setter (Point x _) y' = Point x y'

_points :: Iso' Segment (Point, Point)
_points = iso to' from'
  where
    to' (Segment x y) = (x, y)
    from' (x, y) = Segment x y

{- | Given a segment, fold over all of its points. (If it's not vertical or horizontal it is fine
   to return the extremes)
-}
_fullPoints :: Fold Segment Point
_fullPoints = folding getSegmentLine'

belongsToSegment :: Point -> Segment -> Bool
belongsToSegment (Point x y) (Segment (Point x0 y0) (Point x1 y1)) =
    (x - x0) * (y1 - y0) == (y - y0) * (x1 - x0)

getSegmentLine :: Segment -> [Point]
getSegmentLine s
    | not (isVertical s) && not (isHorizontal s) = s ^.. _points . both
    | otherwise =
        let xFold = _points . both . _x
            yFold = _points . both . _y
            minX = fromJust $ minimumOf xFold s
            maxX = fromJust $ maximumOf xFold s
            minY = fromJust $ minimumOf yFold s
            maxY = fromJust $ maximumOf yFold s
         in [Point x y | x <- [minX .. maxX], y <- [minY .. maxY]]

getSegmentLine' :: Segment -> [Point]
getSegmentLine' s =
    let xFold = _points . both . _x
        yFold = _points . both . _y
        minX = fromJust $ minimumOf xFold s
        maxX = fromJust $ maximumOf xFold s
        minY = fromJust $ minimumOf yFold s
        maxY = fromJust $ maximumOf yFold s
     in [ Point x y
        | x <- [minX .. maxX]
        , y <- [minY .. maxY]
        , Point x y `belongsToSegment` s
        ]

pointParser :: Parser Point
pointParser = Point <$> intParser <* char ',' <*> intParser

segmentParser :: Parser Segment
segmentParser = Segment <$> pointParser <* separator <*> pointParser
  where
    separator :: Parser () = do
        _ <- string " -> "
        pure ()

initialParser :: Parser [Segment]
initialParser = do
    segs <- many . try $ segmentParser <* some newline
    lastSeg <- segmentParser
    _ <- eof
    pure $ segs ++ [lastSeg]

isHorizontal :: Segment -> Bool
isHorizontal (Segment (Point _ y) (Point _ y'))
    | y == y' = True
    | otherwise = False

isVertical :: Segment -> Bool
isVertical (Segment (Point x _) (Point x' _))
    | x == x' = True
    | otherwise = False

type MapState = M.Map Point Int

{- | For a given segment add 1 to the occurency of each point in the state that belong
     to the segment
-}
acc1 :: MapState -> Segment -> MapState
acc1 state segment =
    let allPoints = segment ^.. _fullPoints
     in F.foldl' (flip (M.adjust (+ 1))) state allPoints

initState :: [Segment] -> MapState
initState s =
    let maxX = fromJust $ maximumOf (traversed . _points . both . _x) s
        maxY = fromJust $ maximumOf (traversed . _points . both . _y) s
        points = [Point x y | x <- [0 .. maxX], y <- [0 .. maxY]]
     in M.fromList [(p, 0) | p <- points]

printState :: MapState -> IO ()
printState s = do
    let stateLines = s & M.toList & L.groupBy (\(p, _) (p', _) -> p ^. _x == p' ^. _x)
    forM_ stateLines $ \l -> do
        let vals = l & map snd
            myShow :: Int -> String
            myShow 0 = "."
            myShow x = show x
            chars = concatMap myShow vals
         in putStrLn chars

main :: FilePath -> IO ()
main fp = do
    input <- readFile fp
    segments <- embedMaybe . parseMaybe initialParser $ input
    let horVerSegs = filter (\x -> isHorizontal x || isVertical x) segments
        initialState = initState horVerSegs
        part1State = F.foldl' acc1 initialState horVerSegs
        part1Res =
            part1State
                & M.toList
                & filter ((> 1) . snd)
                & length
    -- printState part1State
    putStrLn $ "Res1: " ++ show part1Res
    let initialState' = initState segments
        part2State = F.foldl' acc1 initialState' segments
        part2Res =
            part2State
                & M.toList
                & filter ((> 1) . snd)
                & length
    putStrLn $ "Res1: " ++ show part2Res
