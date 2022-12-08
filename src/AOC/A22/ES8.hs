{-# LANGUAGE TupleSections #-}

module AOC.A22.ES8 where

import AOC.Utils
import AOC.Utils.Grid
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (concatMap, foldl')
import Data.List (sortBy)
import Data.Maybe (fromJust)
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
        else
          modify' $ \(_, p, lVis', rVis') -> (pos + 1, p, lVis', filter (\(_, a) -> a > cur) rVis' <> [(pos, cur)])
          -- if cur < prev
          --   then modify' $ \(_, prevM, lVis', rVis') -> (pos + 1, prevM, lVis', cur : rVis')
          --   else modify' $ \(_, prevM, lVis', _) -> (pos + 1, prevM, lVis', [cur])

visiblePos :: Ord a => [a] -> [Int]
visiblePos [] = []
visiblePos xs =
  let (l, _, lVis, rVisItems) = execState (forM xs step) (0, Nothing, [], [])
      rVis = map fst rVisItems
   in uniq $ lVis <> rVis

-- Parts

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

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  let parsed = map (map (read @Int . (: []))) inputLines
  grid <- createGrid parsed

  res1 <- part1 grid

  putStrLn $ "A22E8 - part1: " <> show res1