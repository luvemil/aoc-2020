{-# LANGUAGE TupleSections #-}

module AOC.Utils where

import Control.Lens
import qualified Data.Foldable as F
import Data.List (foldl')
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (digitChar)
import Text.Read (readMaybe)

type Parser = Parsec Void String

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

joinWith :: a -> [[a]] -> [a]
joinWith _ [] = []
joinWith _ [xs] = xs
joinWith sep (xs : xss) = xs ++ [sep] ++ joinWith sep xss

embedMaybe :: MonadFail m => Maybe a -> m a
embedMaybe Nothing = fail "Nothing"
embedMaybe (Just x) = pure x

intParser :: Parser Int
intParser = do
    val <- readMaybe <$> many digitChar
    embedMaybe val

signedIntParser :: Parser Int
signedIntParser = do
    sign <- fromMaybe '+' <$> (optional . try $ oneOf ['+', '-'])
    val <- intParser
    case sign of
        '+' -> pure val
        '-' -> pure $ -1 * val
        s -> fail $ "Expected sign to be '-', '+' or nothing, found " <> [s]

arrParser :: Parser a -> Parser b -> Parser [b]
arrParser sep p = do
    initEls <- many . try $ p <* sep
    lastEl <- p
    pure $ initEls ++ [lastEl]

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

getPairs :: [a] -> [(a, a)]
getPairs [] = []
getPairs [_] = []
getPairs (x : xs) = couple x xs ++ getPairs xs
  where
    couple z zs = map (z,) zs

rollingWindows :: Int -> [a] -> [[a]]
rollingWindows t xs
    | length xs < t = []
    | otherwise = take t xs : rollingWindows t (tail xs)

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks t xs = take t xs : chunks t (drop t xs)

splitExact :: Traversable t => t a -> Int -> [[a]]
splitExact xs n
    | n < 0 = error "splitExact x n, x should be >= 0"
    | n == 0 = []
    | otherwise =
        let myHead = xs ^.. taking 5 traversed
            myTail = xs ^.. dropping 5 traversed
         in case length myHead of
                5 -> myHead : splitExact myTail n
                _ -> []

countOccurrencies :: (Ord a, Foldable t, Integral n) => t a -> M.Map a n
countOccurrencies = F.foldl' (flip (M.alter func)) M.empty
  where
    func Nothing = Just 1
    func (Just x) = Just $ x + 1

-- TODO: specify precedence
(!?) :: (Traversable t) => t a -> Int -> Maybe a
xs !? n = xs ^? dropping n traversed