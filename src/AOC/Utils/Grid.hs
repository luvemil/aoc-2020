module AOC.Utils.Grid where

import AOC.Utils (joinWith, uniq, (!?))
import Control.Lens
import Data.Maybe (catMaybes, fromMaybe)

data Grid a = Grid Int Int [a]
    -- { width :: Int
    -- , height :: Int
    -- , points :: [a]
    -- }
    deriving (Show, Eq)

instance Functor Grid where
    fmap f (Grid w h xs) = Grid w h $ map f xs

instance Foldable Grid where
    foldMap f (Grid _ _ xs) = foldMap f xs

instance Traversable Grid where
    traverse f (Grid w h xs) = Grid w h <$> traverse f xs

convertIndex :: Grid a -> ((Int, Int) -> b) -> Int -> b
convertIndex (Grid w _ _) f = f'
  where
    f' i = f (i `mod` w, i `div` w)

instance FunctorWithIndex (Int, Int) Grid where
    imap f grid@(Grid w h xs) = Grid w h $ imap (convertIndex grid f) xs

instance FoldableWithIndex (Int, Int) Grid where
    ifoldMap f grid@(Grid _ _ xs) = ifoldMap (convertIndex grid f) xs
    ifoldr f b grid@(Grid _ _ xs) = ifoldr (convertIndex grid f) b xs
    ifoldr' f b grid@(Grid _ _ xs) = ifoldr' (convertIndex grid f) b xs
    ifoldl f b grid@(Grid _ _ xs) = ifoldl (convertIndex grid f) b xs
    ifoldl' f b grid@(Grid _ _ xs) = ifoldl' (convertIndex grid f) b xs

instance TraversableWithIndex (Int, Int) Grid where
    itraverse f grid@(Grid w h xs) = Grid w h <$> itraverse (convertIndex grid f) xs

_positions :: Fold (Grid a) (Int, Int)
_positions = folding $ \(Grid w h _) -> [(x, y) | x <- [0 .. (w - 1)], y <- [0 .. (h - 1)]]

_withIndex :: Fold (Grid a) (a, (Int, Int))
_withIndex =
    let enriched g =
            g
                ^.. _positions
                    . to (\(x', y') -> (getPosition x' y' g, (x', y')))
                    . to flipMaybe
                    . traversed
        flipMaybe (Nothing, _) = Nothing
        flipMaybe (Just a, p) = Just (a, p)
     in folding $ \grid -> enriched grid

_points :: Traversal (Grid a) (Grid b) a b
-- _points handler (Grid w h xs) = Grid w h <$> traverse handler xs
_points = traverse

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

getSquareNeighborPos :: Int -> Int -> Grid a -> [(Int, Int)]
getSquareNeighborPos x y grid =
    let allPos = [(x + epsilon, y + delta) | epsilon <- [-1, 0, 1], delta <- [-1, 0, 1], epsilon /= 0 || delta /= 0]
     in allPos & filter (`isIn` grid)

getSquareNeighbors :: Int -> Int -> Grid a -> [a]
getSquareNeighbors x y grid
    | not ((x, y) `isIn` grid) = []
    | otherwise = catMaybes $ getSquareNeighborPos x y grid & traversed %~ func
  where
    func (x', y') = getPosition x' y' grid

_sqNbhd :: forall a. (Int, Int) -> Traversal' (Grid a) a
_sqNbhd (x, y) handler grid@(Grid w h as) = Grid w h <$> itraverse h'' as
  where
    nbhd = getSquareNeighborPos x y grid
    h'' i a =
        if (i `mod` w, i `div` w) `elem` nbhd
            then handler a
            else pure a

_isqNbhd :: (Int, Int) -> IndexedTraversal' (Int, Int) (Grid a) a
_isqNbhd (x, y) = itraversed . indices (`elem` nbhdPos)
  where
    nbhdPos = [(x + i, y + j) | i <- [-1, 0, 1], j <- [-1, 0, 1], i /= 0 || j /= 0]

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

-- getIndexedGrid :: forall a m. MonadFail m => Grid a -> m (Grid (a, (Int, Int)))
-- getIndexedGrid grid@(Grid w h _) = createGrid ixedVals
--   where
--     ixedVals :: [[(a, (Int, Int))]] =
--         [ map () | (row, i) <- zip (grid ^.. _rows) [0 ..]]

type instance Index (Grid a) = (Int, Int)
type instance IxValue (Grid a) = a

instance Ixed (Grid a) where
    ix :: (Int, Int) -> Traversal' (Grid a) a
    ix (x, y) handler (Grid w h xs) =
        Grid w h <$> traverseOf theFold handler' xs
      where
        (theFold, handler') =
            if x >= 0 && y >= 0 && x < w && y < h
                then (ix (y * w + x), handler)
                else (traversed, pure)

add :: forall a. Monoid a => Grid a -> Grid a -> Grid a
add grid1@(Grid w h _) grid2@(Grid w' h' _) =
    let w'' = max w w'
        h'' = max h h'
        emptyGrid = Grid w'' h'' [mempty :: a | _ <- [1 .. w'' * h'']]
        extract = fromMaybe mempty
     in emptyGrid & itraversed %@~ \(x, y) _ ->
            extract (getPosition x y grid1) <> extract (getPosition x y grid2)

shift :: forall a. Monoid a => Int -> Int -> Grid a -> Grid a
shift x y grid@(Grid w h _) =
    let w' = w + x
        h' = h + y
        extract = fromMaybe mempty
        emptyGrid = Grid w' h' [mempty :: a | _ <- [1 .. w' * h']]
     in emptyGrid & itraversed %@~ \(i, j) _ ->
            -- extract (getPosition (i - x) (j - y) grid)
            extract (grid ^? ix (i - x, j - y))

shift2 :: forall a. Monoid a => (Int, Int) -> Grid a -> Grid a
shift2 = uncurry shift
