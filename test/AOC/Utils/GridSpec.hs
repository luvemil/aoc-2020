module AOC.Utils.GridSpec where

import AOC.Utils.Grid
import Control.Lens.Combinators
import Control.Lens.Operators
import Data.Maybe (fromJust)
import Data.Monoid (Sum (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck

main :: IO ()
main = hspec spec

myGrid :: Arbitrary a => Gen (Grid a)
myGrid = do
    (w, h) <- arbitrary `suchThat` (\(x, y) -> x > 0 && y > 0)
    -- xs <- sequenceA [arbitrary | _ <- [1 .. w], _ <- [1 .. h]]
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
        prop "add is commutative" $
            prop_commutativeAdd @(Sum Int)
        prop "add is associative" $
            prop_associativeAdd @(Sum Int)
        prop "shift -x (shift x grid) is id" $
            prop_commutativeShift @(Sum Int)
  where
    [g1, g1Shifted, g2, g3, g4, g5, g6] =
        map
            makeGrid
            [g1Els, g1ShiftedEls, g2Els, g3Els, g4Els, g5Els, g6Els]
