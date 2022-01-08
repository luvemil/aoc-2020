module AOC.A21.ES17 where

import AOC.Utils
import Control.Lens
import qualified Data.Foldable as F
import Data.Ord (comparing)
import Text.Megaparsec (eof, parseMaybe)
import Text.Megaparsec.Char (string)

type P = (Int, Int)
type Q = (Int, Int)
type S = (P, Q)
type Target = ((Int, Int), (Int, Int))

parseTarget :: Parser Target
parseTarget = do
    _ <- string "target area: "
    x0 <- string "x=" *> signedIntParser <* string ".."
    x1 <- signedIntParser <* string ", "
    y0 <- string "y=" *> signedIntParser <* string ".."
    y1 <- signedIntParser <* eof
    pure ((x0, x1), (y0, y1))

isInTarget :: P -> Target -> Bool
isInTarget (x, y) ((xs, xe), (ys, ye)) = x >= xs && x <= xe && y >= ys && y <= ye

getAdmissiblePaths :: (P -> Q -> Target -> [S]) -> Target -> IO [[S]]
getAdmissiblePaths cfp t@((xs, xe), (ys, ye)) = do
    let maxX = max (abs xs) (abs xe)
        maxY = max (abs ys) (abs ye)
        admissibleX = [x | x <- [- maxX .. maxX]]
        admissibleY = [y | y <- [- maxY .. maxY]]
        admissibleVelocities = [(x, y) | x <- admissibleX, y <- admissibleY]
    -- putStrLn $ "admissibleVelocities: " <> show admissibleVelocities
    let allPaths =
            admissibleVelocities
                & map (\q -> cfp (0, 0) q t)
                & filter (not . null)
    -- putStrLn $ "allPaths: " <> show allPaths
    pure $ filter (\ps -> fst (last ps) `isInTarget` t) allPaths

nextPoint :: P -> Q -> S
nextPoint (px, py) (qx, qy) =
    ( (px + qx, py + qy)
    , (f qx, qy - 1)
    )
  where
    f x = signum x * (abs x - 1)

computePath :: P -> Q -> [S]
computePath p q = (p, q) : computePath p' q'
  where
    (p', q') = nextPoint p q

-- TODO: fix in case (xs, xe) is negative, or (ys, ye) is positive
computePathUpTo :: P -> Q -> Target -> [S]
computePathUpTo p q ((xs, xe), (ys, ye)) =
    let maxX = max xs xe
        minY = min ys ye
     in takeWhile (\((x, y), _) -> x <= maxX && y >= minY) $ computePath p q

part1 :: Target -> IO Int
part1 t = do
    admissiblePaths <- getAdmissiblePaths computePathUpTo t
    -- putStrLn $ "Admissible paths: " <> show admissiblePaths
    let maximumPath = F.maximumBy (comparing (maximumOf (traversed . _1 . _2))) admissiblePaths
    putStrLn $ "Maximum path: " <> show maximumPath
    let highestPos = maximum $ map (maximumOf (traversed . _1 . _2)) admissiblePaths
    embedMaybe highestPos

part2 :: Target -> IO Int
part2 t = do
    admissiblePaths <- getAdmissiblePaths computePathUpTo t
    pure $ length admissiblePaths

main :: FilePath -> IO ()
main fp = do
    input <- readFile fp
    target <- embedMaybe . parseMaybe parseTarget $ input
    putStrLn $ "Target: " <> show target
    res1 <- part1 target
    putStrLn $ "Res1: " <> show res1
    res2 <- part2 target
    putStrLn $ "Res2: " <> show res2