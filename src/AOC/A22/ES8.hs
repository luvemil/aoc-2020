{-# LANGUAGE TupleSections #-}

module AOC.A22.ES8 where

import AOC.Utils
import AOC.Utils.Grid
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (concatMap, foldl')
import qualified Data.Foldable as L
import Data.List (sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)

type Comp a =
  StateT
    ( Int, -- position
      Maybe a, -- max prev
      [Int], -- visible from left
      [(Int, a)] -- visible from right up to now
    )
    Identity

addToLeftVisible :: Int -> Comp a ()
addToLeftVisible pos = do
  modify' $ over _3 (\s -> s <> [pos])

step :: Ord a => a -> Comp a ()
step cur = do
  (pos, prevM, _, _) <- get
  if pos == 0
    then put (1, Just cur, [0], [(0, cur)])
    else do
      let prev = fromJust prevM
      if cur > prev
        then do
          modify' $ \(_, _, lVis', _) -> (pos + 1, Just cur, lVis' <> [pos], [(pos, cur)])
        else modify' $ \(_, p, lVis', rVis') -> (pos + 1, p, lVis', filter (\(_, a) -> a > cur) rVis' <> [(pos, cur)])

-- if cur < prev
--   then modify' $ \(_, prevM, lVis', rVis') -> (pos + 1, prevM, lVis', cur : rVis')
--   else modify' $ \(_, prevM, lVis', _) -> (pos + 1, prevM, lVis', [cur])

visiblePos :: Ord a => [a] -> [Int]
visiblePos [] = []
visiblePos xs =
  let (l, _, lVis, rVisItems) = execState (forM xs step) (0, Nothing, [], [])
      rVis = map fst rVisItems
   in uniq $ lVis <> rVis

-- Stupid implementation: given an index compute its visibility
rowVisibility :: Ord a => Int -> [a] -> (Int, Int)
rowVisibility pos xs =
  let val = fromJust $ xs !? pos
      xs' =
        xs
          & (itraversed %@~ \i x -> (i, x >= val))
          & filter snd
          & map fst
      lowIx = maximum $ filter (< pos) xs' <> [0]
      highIx = minimum $ filter (> pos) xs' <> [length xs - 1]
   in (pos - lowIx, highIx - pos)

rowVisibilities :: Ord a => [a] -> [(Int, Int)]
rowVisibilities xs = xs & itraversed %@~ \i _ -> rowVisibility i xs

allRowVisiblities :: Ord a => Grid a -> Grid Int
allRowVisiblities grid =
  let byRow = map rowVisibilities $ grid ^.. _rows
      byCol = map rowVisibilities $ grid ^.. _cols
   in grid & itraversed %@~ \(x, y) _ -> 
        let br = byRow & preview (ix y) & preview (_Just . ix x) & fromJust & uncurry (*)
            bc = byCol & preview (ix x) & preview (_Just . ix y) & fromJust & uncurry (*)
         in br * bc

-- Parts
part2 :: Grid Int -> IO Int
part2 g = do
  let arv = allRowVisiblities g
  pure . fromJust $ maximumOf traversed arv

part1 :: Grid Int -> IO Int
part1 g@(Grid w h _) = do
  let visByCol :: [(Int, Int)] =
        (g ^.. _cols)
          & itraversed %@~ (\x c -> map (x,) (visiblePos c))
          & concat
      visByRow =
        (g ^.. _rows)
          & itraversed %@~ (\y r -> map (,y) (visiblePos r))
          & concat
      allVisible = uniq $ visByCol <> visByRow
      visByCol' = g ^.. _cols . to visiblePos
      visByRow' = g ^.. _rows . to visiblePos
  -- putStrLn "visible positions by column"
  -- forM_ visByCol' print
  -- putStrLn "----"
  -- putStrLn "visible positions by row"
  -- forM_ visByRow' print
  -- putStrLn "----"
  pure $ length allVisible

printGridEdge :: Grid a -> (Int, Int) -> Char
printGridEdge _ (0, 0) = '+'
printGridEdge (Grid w _ _) (x, 0) = if x == w - 1 then '+' else '-'
printGridEdge (Grid _ h _) (0, y) = if y == h - 1 then '+' else '|'
printGridEdge (Grid w h _) (x, y)
  | x == w - 1 && y == h - 1 = '+'
  | x == w - 1 = '|'
  | y == h - 1 = '-'
  | otherwise = ' '

printLevels :: Grid Int -> IO ()
printLevels grid = do
  forM_ (reverse [0 .. 9]) $ \size -> do
    let g9 =
          grid & itraversed %@~ \i x ->
            if x >= size
              then 'X'
              else printGridEdge grid i
    putStrLn $ "=== Size " <> show size <> " ==="
    putStrLn $ prettyGrid g9

printProfile :: Int -> [Int] -> IO ()
printProfile l xs = do
  let initialMap :: M.Map Int [Int]
      initialMap = M.fromList $ map (,[]) [0 .. 9]
      initialString = [' ' | _ <- [0 .. l]]
      heights =
        xs
          & itraversed %@~ (\i x -> (x, i))
          & L.foldl'
            ( \acc (i, x) ->
                let base = acc & M.insertWith (\nv ov -> ov <> nv) i [x]
                 in L.foldl' (\acc' j -> M.insertWith (\nv ov -> ov <> nv) j [x] acc') base [0 .. i]
            )
            initialMap

  forM_ (reverse [0 .. 9]) $ \size -> do
    let positions = fromMaybe [] $ M.lookup size heights
        finalS = L.foldl' (\acc cur -> acc & ix cur .~ 'x') initialString positions
    putStrLn finalS

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  let parsed = map (map (read @Int . (: []))) inputLines
  grid <- createGrid parsed

  res1 <- part1 grid
  putStrLn $ "A22E8 - part1: " <> show res1

  -- Print the levels on x-y
  -- printLevels grid
  -- let (Grid w _ _) = grid
  --     items = grid ^.. _cols . ix 15
  -- printProfile w items

  res2 <- part2 grid
  putStrLn $ "A22E8 - part2: " <> show res2
