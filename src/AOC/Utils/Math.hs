module AOC.Utils.Math where

import Control.Lens
import Data.List (sort)

getMean :: (RealFrac a) => [a] -> a
getMean xs = sum xs / fromIntegral (length xs)

meanOf :: (RealFrac a) => Fold s a -> s -> a
meanOf theFold s = sumOf theFold s / fromIntegral (lengthOf theFold s)

getNextNewton :: (RealFrac a) => (a -> a) -> (a -> a) -> a -> a
getNextNewton f f' x0 = x0 - (f x0 / f' x0)

getHarmonicMean :: (RealFrac a) => [a] -> a
getHarmonicMean xs =
    let xs' = [1 / x | x <- xs]
        m = getMean xs'
     in 1 / m

harmonicMeanOf :: (RealFrac a) => Fold s a -> s -> a
harmonicMeanOf theFold s = 1 / meanOf (theFold . to (1 /)) s

medianOf :: Ord a => Fold s a -> s -> Maybe a
medianOf theFold s =
    let l = lengthOf theFold s
        listed = (s ^.. theFold & sort) ^? dropping (floor @Double (fromIntegral l / 2)) folded
     in listed