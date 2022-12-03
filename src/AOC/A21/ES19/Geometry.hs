{-# LANGUAGE TemplateHaskell #-}
module AOC.A21.ES19.Geometry where

import Control.Lens

-- | This is a vector in R^3
data VPoint a = VPoint
  { pX :: !a
  , pY :: !a
  , pZ :: !a
  }
  deriving (Show, Eq)

isZeroVector :: (Num a, Eq a) => VPoint a -> Bool
isZeroVector (VPoint 0 0 0) = True
isZeroVector _ = False

makeLenses ''VPoint

pointVector :: Iso (VPoint a) (VPoint b) (a, a, a) (b, b, b)
-- pointVector :: Iso' (VPoint a) (a, a, a)
pointVector = iso fromPoint toPoint
    where
        fromPoint :: VPoint a -> (a, a, a)
        fromPoint (VPoint x y z) = (x, y, z)
        toPoint :: (b, b, b) -> VPoint b
        toPoint (x, y, z) = VPoint x y z 

type Orientation a = VPoint (VPoint a)
