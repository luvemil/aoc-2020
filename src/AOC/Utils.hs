module AOC.Utils where

import Data.List (foldl')

uniq :: Eq a => [a] -> [a]
uniq = foldl' add []
  where
    add as a = if a `elem` as then as else a : as

-- equal to `forM input parser`
parseList :: MonadFail m => (a -> m b) -> [a] -> m [b]
parseList parser input = do
    let loop (x : xs) = do
            y <- parser x
            ys <- loop xs
            pure $ y : ys
        loop [] = pure []
    loop input

{- | Source: https://hackage.haskell.org/package/haskell-gi-0.24.7/docs/src/Data.GI.CodeGen.Util.html#splitOn
 | Split a list into sublists delimited by the given element.
-}
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = go xs []
  where
    go [] acc = [reverse acc]
    go (y : ys) acc =
        if x == y
            then reverse acc : go ys []
            else go ys (y : acc)

embedMaybe :: MonadFail m => Maybe a -> m a
embedMaybe Nothing = fail "Nothing"
embedMaybe (Just x) = pure x

-- findFixedPointIter :: ((a, b) -> (a, b) -> Bool) -> (a -> b -> (a, b)) -> a -> b -> (a, b)
-- findFixedPointIter isFixed f x y =
--     let (x', y') = f x y
--      in if isFixed (x, y) (x', y')
--             then (x', y')
--             else findFixedPointIter isFixed f x' y'

findFixedPointIter :: (a -> a -> Bool) -> (a -> a) -> a -> a
findFixedPointIter isFixed f x =
    let x' = f x
     in if isFixed x x'
            then x'
            else findFixedPointIter isFixed f x'