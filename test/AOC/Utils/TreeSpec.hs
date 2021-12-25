module AOC.Utils.TreeSpec where

import AOC.Utils.Tree
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

spec :: Spec
{-# NOINLINE spec #-}
spec =
    describe "Tree Operations" $ do
        it "dummy" $
            True `shouldBe` True