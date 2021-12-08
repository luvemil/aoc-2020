module AOC.Utils.Math where

import Control.Lens

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
