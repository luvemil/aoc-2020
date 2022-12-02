module AOC.A21.ES18Spec where

import AOC.A21.ES18
import Test.Hspec
import Control.Lens

main :: IO ()
main = hspec spec

refEl :: SNum Int
refEl = SPair (SPair (SRegular 4) (SRegular 5)) (SPair (SRegular 6) (SRegular 7))

spec :: Spec
{-# NOINLINE spec #-}
spec =
  describe "operations" $ do
    it "split" $
      map sSplit [SRegular 10, SRegular 11, SRegular 12]
        `shouldBe` [ SPair (SRegular 5) (SRegular 5),
                     SPair (SRegular 5) (SRegular 6),
                     SPair (SRegular 6) (SRegular 6)
                   ]
    it "gets all values" $ do
        (refEl ^.. tSVal) `shouldBe` [4, 5, 6, 7]