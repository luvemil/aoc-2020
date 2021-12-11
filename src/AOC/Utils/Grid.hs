module AOC.Utils.Grid where

import AOC.Utils (joinWith, uniq, (!?))
import Control.Lens

data Grid a = Grid Int Int [a]
    -- { width :: Int
    -- , height :: Int
    -- , points :: [a]
    -- }
    deriving (Show, Eq)

instance Functor Grid where
    fmap f (Grid w h xs) = Grid w h (map f xs)

_positions :: Fold (Grid a) (Int, Int)
_positions = folding $ \(Grid w h _) -> [(x, y) | x <- [0 .. (w - 1)], y <- [0 .. (h - 1)]]

_points :: Traversal (Grid a) (Grid b) a b
_points handler (Grid w h xs) = Grid w h <$> traverse handler xs

getPosition :: Int -> Int -> Grid a -> Maybe a
getPosition x y (Grid w h xs)
    | x < 0 || y < 0 || x >= w || y >= h = Nothing
    | otherwise = let n = w * y + x in xs !? n

getRow :: Int -> Grid a -> [a]
getRow x (Grid w h xs)
    | x < 0 || x > h = []
    | otherwise = xs ^.. (taking w . dropping (x * w)) traversed

_rows :: Fold (Grid a) [a]
_rows = folding $ \x@(Grid _ h _) -> map (`getRow` x) [0 .. (h - 1)]

-- | Output: (Bottom, Right, Top, Left)
getNeighbors :: Int -> Int -> Grid a -> (Maybe a, Maybe a, Maybe a, Maybe a)
getNeighbors x y grid@(Grid w h _)
    | x < 0 || y < 0 || x >= w || y >= h = (Nothing, Nothing, Nothing, Nothing)
    | otherwise =
        ( getPosition x (y + 1) grid
        , getPosition (x + 1) y grid
        , getPosition x (y - 1) grid
        , getPosition (x - 1) y grid
        )

createGrid :: MonadFail m => [[a]] -> m (Grid a)
createGrid xss = do
    let ls = uniq $ map length xss
    case ls of
        [l] -> let h = length xss in pure $ Grid l h (concat xss)
        _ -> fail "Different lengths"

-- TODO: define the following
showGrid :: Show a => Grid a -> String
showGrid grid =
    let gridRows = grid ^.. _rows . to showRow
        showRow [] = ""
        showRow (x : xs) = show x ++ showRow xs
     in joinWith '\n' gridRows