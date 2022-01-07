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

computeSpeed :: Q -> Int -> [Q]
computeSpeed (v0x, v0y) n =
    let v0xSign = signum v0x
        v0xs' = [v0x - (i * v0xSign) | i <- [0 .. min (abs v0x) n]]
        v0xs'' = [last v0xs' | _ <- [min (abs v0x) n + 1 .. n]]
        v0ys = [v0y - i | i <- [0 .. n]]
     in zip (v0xs' ++ v0xs'') v0ys

updatePos :: P -> Q -> P
updatePos (x, y) (vx, vy) = (x + vx, y + vy)

updatePosition :: P -> Q -> Int -> [P]
updatePosition (x, y) q n =
    let vs = computeSpeed q n
     in F.foldl' (\acc cur -> acc ++ [last acc `updatePos` cur]) [(x, y)] vs

fullUpdatePosition ::
    -- | Initial position
    P ->
    -- | Initial velocity
    Q ->
    -- | Step to compute
    Int ->
    -- | (Position, Velocity)
    S
fullUpdatePosition p q 0 = (p, q)
fullUpdatePosition p q n = last $ zip (updatePosition p q n) (computeSpeed q n)

-- let v0xSign = signum v0x
--     v0xs = [v0x - (i * v0xSign) | i <- [0 .. min (abs v0x) n]]
--     v0ys = [v0y - i | i <- [0 .. n]]
--  in (
--         ( x0 + sum v0xs
--         , y0 + sum v0ys
--         )
--     ,
--         ( last v0xs
--         , last v0ys
--         )
--     )

computeFullPath :: P -> Q -> Target -> [S]
computeFullPath p@(x, y) q t@((xs, xe), (ys, ye)) =
    if x > max xs xe || y < min ys ye
        then []
        else
            let (p', q') = fullUpdatePosition p q 1
             in (p, q) : computeFullPath p' q' t

isInTarget :: P -> Target -> Bool
isInTarget (x, y) ((xs, xe), (ys, ye)) = x >= xs && x <= xe && y >= ys && y <= ye

getAdmissiblePaths :: Target -> IO [[S]]
getAdmissiblePaths t@((xs, xe), (ys, ye)) = do
    let maxX = max (abs xs) (abs xe)
        maxY = max (abs ys) (abs ye)
        admissibleX = [x | x <- [- maxX .. maxX]]
        admissibleY = [y | y <- [- maxY .. maxY]]
        admissibleVelocities = [(x, y) | x <- admissibleX, y <- admissibleY]
    -- putStrLn $ "admissibleVelocities: " <> show admissibleVelocities
    let allPaths =
            admissibleVelocities
                & map (\q -> computeFullPath (0, 0) q t)
                & filter (not . null)
    -- putStrLn $ "allPaths: " <> show allPaths
    pure $ filter (\ps -> fst (last ps) `isInTarget` t) allPaths

part1 :: Target -> IO Int
part1 t = do
    admissiblePaths <- getAdmissiblePaths t
    -- putStrLn $ "Admissible paths: " <> show admissiblePaths
    let maximumPath = F.maximumBy (comparing (maximumOf (traversed . _1 . _2))) admissiblePaths
    putStrLn $ "Maximum path: " <> show maximumPath
    let highestPos = maximum $ map (maximumOf (traversed . _1 . _2)) admissiblePaths
    embedMaybe highestPos

part2 :: Target -> IO Int
part2 t = do
    admissiblePaths <- getAdmissiblePaths t
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