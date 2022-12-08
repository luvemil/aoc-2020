module AOC.Utils.GridSpec where

import AOC.Utils.Grid
import Control.Lens.Combinators
import Control.Lens.Operators
import qualified Data.List as L
import Data.Maybe (catMaybes, fromJust)
import Data.Monoid (Sum (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec spec

myGrid :: Arbitrary a => Gen (Grid a)
myGrid = do
    -- Bound the size to grids of 400 entries (x * y <= 400)
    (w, h) <- arbitrary `suchThat` (\(x, y) -> x > 0 && y > 0 && x <= 5 && y <= 5)
    xs <- vector (w * h)
    pure $ Grid w h xs

instance Arbitrary a => Arbitrary (Grid a) where
    arbitrary = myGrid

prop_commutativeAdd :: (Monoid a, Eq a) => Grid a -> Grid a -> Bool
prop_commutativeAdd x y = x `add` y == y `add` x

prop_associativeAdd :: (Monoid a, Eq a) => Grid a -> Grid a -> Grid a -> Bool
prop_associativeAdd x y z = (x `add` y) `add` z == x `add` (y `add` z)

prop_commutativeShift :: (Monoid a, Eq a) => (Int, Int) -> Grid a -> Bool
prop_commutativeShift (x, y) grid = shift (- x') (- y') (shift x' y' grid) == grid
  where
    x' = abs x
    y' = abs y

prop_getPositionEqualsIx :: Eq a => (Int, Int) -> Grid a -> Bool
prop_getPositionEqualsIx (x, y) grid = grid ^? ix (x, y) == getPosition x y grid

prop_sqNbhdWithAndWithoutIndices :: Ord a => (Int, Int) -> Grid a -> Bool
prop_sqNbhdWithAndWithoutIndices (x, y) grid =
    let nbhd1 = grid ^.. _sqNbhd (x, y)
        nbhd2 = grid ^.. _isqNbhd (x, y)
     in L.sort nbhd1 == L.sort nbhd2

prop_sqNbhdPos :: (Int, Int) -> Grid a -> Bool
prop_sqNbhdPos (x, y) grid =
    let nbhdPos1 = map fst $ grid ^@.. _isqNbhd (x, y)
        nbhdPos2 = getSquareNeighborPos x y grid
     in L.sort nbhdPos1 == L.sort nbhdPos2

prop_doubleFlipV :: Eq a => Grid a -> Bool
prop_doubleFlipV grid = (flipV . flipV) grid == grid

prop_doubleFlipH :: Eq a => Grid a -> Bool
prop_doubleFlipH grid = (flipH . flipH) grid == grid

prop_nbhdVals :: Ord a => (Int, Int) -> Grid a -> Bool
prop_nbhdVals (x, y) grid =
    let nbhd1 = grid ^.. _inbhd (x, y)
        nbhd2 = catMaybes $ getNeighbors x y grid ^.. each
     in L.sort nbhd1 == L.sort nbhd2

prop_concatHShiftAdd :: (Eq a, Monoid a) => Grid a -> Grid a -> Bool
prop_concatHShiftAdd g1@(Grid w1 h1 _) g2@(Grid w2 h2 _) =
    g1 `concatGridH` g2
        == g1 `add` shift w1 0 g2

prop_concatVShiftAdd :: (Eq a, Monoid a) => Grid a -> Grid a -> Bool
prop_concatVShiftAdd g1@(Grid w1 h1 _) g2@(Grid w2 h2 _) =
    g1 `concatGridV` g2
        == g1 `add` shift 0 h1 g2

-- Some matrix data
g1Els :: [[Integer]]
g1Els =
    [ [1, 2, 3]
    , [4, 5, 6]
    ]

g1ShiftedEls :: [[Integer]]
g1ShiftedEls =
    [ [0, 0, 0, 0]
    , [0, 0, 0, 0]
    , [0, 1, 2, 3]
    , [0, 4, 5, 6]
    ]

g2Els :: [[Integer]]
g2Els =
    [[44]]

-- g1 + shift 0 1 g2
g3Els :: [[Integer]]
g3Els =
    [ [1, 2, 3]
    , [48, 5, 6]
    ]

-- g1 + shift 1 0 g2
g4Els :: [[Integer]]
g4Els =
    [ [1, 46, 3]
    , [4, 5, 6]
    ]

-- g1 + shift 1 1 g2
g5Els :: [[Integer]]
g5Els =
    [ [1, 2, 3]
    , [4, 49, 6]
    ]

g5Els' :: [[Integer]]
g5Els' = 
    [ [1, 4]
    , [2, 49]
    , [3, 6] 
    ]

-- g1 + shift 1 2 g2
g6Els :: [[Integer]]
g6Els =
    [ [1, 2, 3]
    , [4, 5, 6]
    , [0, 44, 0]
    ]

makeGrid :: [[a]] -> Grid (Sum a)
makeGrid xs = fromJust (createGrid xs) & traversed %~ Sum

spec :: Spec
{-# NOINLINE spec #-}
spec =
    describe "Grid Operations" $ do
        it "shifts" $
            shift 1 2 g1 `shouldBe` g1Shifted
        it "g1 + shift 0 1 g2" $
            add g1 (shift 0 1 g2) `shouldBe` g3
        it "g1 + shift 1 0 g2" $
            add g1 (shift 1 0 g2) `shouldBe` g4
        it "g1 + shift 1 1 g2" $
            add g1 (shift 1 1 g2) `shouldBe` g5
        it "g1 + shift 1 2 g2" $
            add g1 (shift 1 2 g2) `shouldBe` g6
        it "cols gets the columns" $
            head (toListOf _cols g6) `shouldBe` ([1, 4, 0] :: [Sum Integer])
        it "the positions (x, y) is from top left" $
            (g6 ^? ix (0, 2)) `shouldBe` Just (0 :: Sum Integer)
        prop "add is commutative" $
            prop_commutativeAdd @(Sum Int)
        prop "add is associative" $
            prop_associativeAdd @(Sum Int)
        prop "shift -x (shift x grid) is id" $
            prop_commutativeShift @(Sum Int)
        prop "ix (i, j) is the same as getPosition i j" $
            prop_getPositionEqualsIx @Int
        prop "_sqNbhd == _isqNbhd" $
            prop_sqNbhdWithAndWithoutIndices @Int
        prop "_isqNbhd == getSquareNeiborhoodPos" $
            prop_sqNbhdPos @Bool
        prop "flipV . flipV == id" $
            prop_doubleFlipV @Int
        prop "flipH . flipH == id" $
            prop_doubleFlipH @Int
        -- prop "_inbhd" $
        --     prop_nbhdVals @Int
        prop "concatGridH" $
            prop_concatHShiftAdd @(Sum Int)
        prop "concatGridV" $
            prop_concatVShiftAdd @(Sum Int)
  where
    [g1, g1Shifted, g2, g3, g4, g5, g5', g6] =
        map
            makeGrid
            [g1Els, g1ShiftedEls, g2Els, g3Els, g4Els, g5Els, g5Els', g6Els]
