module AOC.A21.ES19.Objects where

import AOC.A21.ES19.AbstractObjects
import AOC.A21.ES19.Geometry
import Control.Lens

type CBeacon = Beacon (VPoint Int)

instance Show CBeacon where
  show (Beacon vec) =
    let tr :: (Int, Int, Int) = vec ^. pointVector
     in show tr

-- Orientation

type COrientation = Orientation Int

type CVector = VPoint Int

allNormalVectors :: [CVector]
allNormalVectors = do
  x <- [-1, 0, 1]
  y <- [-1, 0, 1]
  z <- [-1, 0, 1]
  let vec = (x, y, z) ^. from pointVector
  [vec | not (isZeroVector vec)]

allOrientations :: [COrientation]
allOrientations =
  [ (vX, vY, vZ)
      ^. from pointVector
    | vX <- allNormalVectors,
      vY <- allNormalVectors,
      vZ <- allNormalVectors
  ]
