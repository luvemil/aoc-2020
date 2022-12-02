module AOC.A21.ES18Spec where

import AOC.A21.ES18
import Control.Lens
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Text.Megaparsec (parseMaybe)
import Data.Maybe (mapMaybe, isJust)
import Control.Monad ((<=<), forM_)
import Data.Foldable (Foldable(foldl'), foldlM)
import AOC.Utils (embedMaybe)

main :: IO ()
main = hspec spec

sRefEl :: String
sRefEl = "[[4,5],[6,7]]"

refEl :: SNum Int
refEl = SPair (SPair (SRegular 4) (SRegular 5)) (SPair (SRegular 6) (SRegular 7))

sRefEl2 :: String
sRefEl2 = "[[[[4,5],6],[7,8]],9]"

refEl2 :: SNum Int
refEl2 =
  SPair
    ( SPair
        ( SPair
            (SPair (SRegular 4) (SRegular 5))
            (SRegular 6)
        )
        ( SPair
            (SRegular 7)
            (SRegular 8)
        )
    )
    (SRegular 9)

refEl2Base :: [SBaseNum Int]
refEl2Base = [SBasePair 4 5, SBaseReg 6, SBasePair 7 8, SBaseReg 9]

refEl2BaseIxed :: [(Int, SBaseNum Int)]
refEl2BaseIxed = [(3, SBasePair 4 5), (3, SBaseReg 6), (2, SBasePair 7 8), (1, SBaseReg 9)]

sRegNum :: Gen (SNum Int)
sRegNum = SRegular <$> chooseInt (0, 9)

sNumDepth :: Int -> Gen (SNum Int)
sNumDepth x
  | x < 0 =
    sNumDepth 0
  | x > 4 = sNumDepth 4
  | x == 0 = sRegNum
  | otherwise = do
    l <- sNumDepth (x - 1)
    r <- sNumDepth (x - 1)
    pure $ SPair l r

instance Arbitrary (SNum Int) where
  arbitrary = sized sNumDepth

prop_indexedSameAsTrav :: SNum Int -> Bool
prop_indexedSameAsTrav snum =
  let iVal = snum ^.. itSPairVal
      tVal = snum ^.. tSPairVal
   in iVal == tVal

prop_regSameAsNaive :: SNum Int -> Bool
prop_regSameAsNaive snum =
  let naive = snum ^.. tSVal . to SBaseReg
      reg = snum ^.. tSRegVal
    in naive == reg

opsVals :: [(String, ReduceOp)]
opsVals = [
    ("[[[[[9,8],1],2],3],4]", Explode 0),
    ("[7,[6,[5,[4,[3,2]]]]]", Explode 4),
    ("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", Explode 6),
    ("[[3,[2,[8,0]]],[10,[5,[7,0]]]]", Split 4)
  ]

fromString :: MonadFail m => String -> m (SNum Int)
fromString = embedMaybe . parseMaybe sNumParser

sumsVals :: [([String], String)]
sumsVals =
  [ (["[1,1]","[2,2]","[3,3]","[4,4]"], "[[[[1,1],[2,2]],[3,3]],[4,4]]")
  , (["[1,1]","[2,2]","[3,3]","[4,4]","[5,5]"], "[[[[3,0],[5,3]],[4,4]],[5,5]]")
  , (["[1,1]","[2,2]","[3,3]","[4,4]","[5,5]","[6,6]"], "[[[[5,0],[7,4]],[5,5]],[6,6]]")
  ]

spec :: Spec
{-# NOINLINE spec #-}
spec =
  describe "operations" $ do
    it "sSplit splits correctly" $
      map sSplit [SRegular 10, SRegular 11, SRegular 12]
        `shouldBe` [ SPair (SRegular 5) (SRegular 5),
                     SPair (SRegular 5) (SRegular 6),
                     SPair (SRegular 6) (SRegular 6)
                   ]
    it "sBaseSplit splits correctly" $
      map sBaseSplit [SBaseReg 10, SBaseReg 11, SBaseReg 12]
        `shouldBe` [SBasePair 5 5, SBasePair 5 6, SBasePair 6 6]
    it "tSVal gets all values" $ do
      (refEl ^.. tSVal) `shouldBe` [4, 5, 6, 7]
    it "tSPairVal gets all values" $ do
      (refEl2 ^.. tSPairVal) `shouldBe` refEl2Base
    it "itSPairVal gets values with depth" $ do
      (refEl2 ^@.. itSPairVal) `shouldBe` refEl2BaseIxed
    it "parses el1" $ do
      parseMaybe sNumParser sRefEl `shouldBe` Just refEl
    it "parses el2" $ do
      parseMaybe sNumParser sRefEl2 `shouldBe` Just refEl2
    prop "tSPairVal is the same as itSPairVal" $ do
      prop_indexedSameAsTrav
    prop "tSRegVal is the same as (tSVal . to SBaseReg)" $ do
      prop_regSameAsNaive
    it "explodeAt worked" $ do
      el <- fromString "[[[[0,[3,2]],[3,3]],[4,4]],[5,5]]"
      res <- fromString "[[[[3,0],[5,3]],[4,4]],[5,5]]"
      pos1Res <- fromString "[3,2]"
      let vals = el ^.. tSPairVal
      vals `shouldBe` [SBaseReg 0, SBasePair 3 2, SBasePair 3 3, SBasePair 4 4, SBasePair 5 5]
      let pos1 = vals ^? taking 1 (dropping 1 traversed)
      liftSNum <$> pos1 `shouldBe` Just pos1Res
      sNumExplodeAt 1 el `shouldBe` res
    it "nextOp works" $ do
      mapMaybe (Just . nextOp <=< parseMaybe sNumParser . fst) opsVals `shouldBe` map snd opsVals
    it "sAdd trivial works" $ do
      sn1 <- embedMaybe $ parseMaybe sNumParser "[1,1]"
      sn2 <- embedMaybe $ parseMaybe sNumParser "[2,2]"
      res <- embedMaybe $ parseMaybe sNumParser "[[1,1],[2,2]]"
      sAddRes <- sAdd sn1 sn2
      sAddRes `shouldBe` res
    forM_ sumsVals $ \(sNumsStr, sResStr) ->
      it ("sAdd: " <> show sNumsStr <> " -> " <> sResStr) $ do
        let sNums = mapMaybe (parseMaybe sNumParser) sNumsStr
        sRes <- embedMaybe $ parseMaybe sNumParser sResStr
        sSum <- foldlM sAdd (head (take 1 sNums)) (drop 1 sNums)
        sSum `shouldBe` sRes
