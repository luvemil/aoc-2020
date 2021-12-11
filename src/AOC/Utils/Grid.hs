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

isIn :: (Int, Int) -> Grid a -> Bool
(x, y) `isIn` (Grid w h _) = x >= 0 && y >= 0 && x < w && y < h

getNeighborPos :: Int -> Int -> Grid a -> (Maybe (Int, Int), Maybe (Int, Int), Maybe (Int, Int), Maybe (Int, Int))
getNeighborPos x y grid =
    let validate pos =
            if pos `isIn` grid
                then Just pos
                else Nothing
     in ((x, y + 1), (x + 1, y), (x, y -1), (x - 1, y)) & each %~ validate

-- | Output: (Bottom, Right, Top, Left)
getNeighbors :: Int -> Int -> Grid a -> (Maybe a, Maybe a, Maybe a, Maybe a)
getNeighbors x y grid
    | not ((x, y) `isIn` grid) = (Nothing, Nothing, Nothing, Nothing)
    | otherwise = getNeighborPos x y grid & each %~ (func =<<)
  where
    func (x', y') = getPosition x' y' grid

createGrid :: MonadFail m => [[a]] -> m (Grid a)
createGrid xss = do
    let ls = uniq $ map length xss
    case ls of
        [l] -> let h = length xss in pure $ Grid l h (concat xss)
        _ -> fail "Different lengths"

showGrid :: Show a => Grid a -> String
showGrid grid =
    let gridRows = grid ^.. _rows . to showRow
        showRow [] = ""
        showRow (x : xs) = show x ++ showRow xs
     in joinWith '\n' gridRows