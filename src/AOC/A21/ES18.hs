{-# LANGUAGE TemplateHaskell #-}

module AOC.A21.ES18 where

import AOC.Utils (Parser, embedMaybe, parseList)
import Control.Applicative ((<|>))
import Control.Lens
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Text.Megaparsec (MonadParsec (try), many, parseMaybe)
import Text.Megaparsec.Char (char, digitChar)
import Text.Read (readMaybe)
import Data.Foldable (foldlM)

-- Type definitions and accessors

data SNum a = SRegular !a | SPair !(SNum a) !(SNum a)
  deriving (Show, Eq)

data SBaseNum a = SBaseReg !a | SBasePair !a !a
  deriving (Show, Eq)

printSNum :: Show a => SNum a -> String
printSNum (SRegular x) = show x
printSNum (SPair x y) = "["<>printSNum x<> ","<>printSNum y<>"]"

makePrisms ''SBaseNum

liftSNum :: SBaseNum a -> SNum a
liftSNum (SBaseReg x) = SRegular x
liftSNum (SBasePair x y) = SPair (SRegular x) (SRegular y)

tSVal :: Traversal (SNum a) (SNum b) a b -- Applicative f => (a -> f b) -> (SNum a) -> f (SNum b)
tSVal f (SRegular x) = SRegular <$> f x
tSVal f (SPair x y) = SPair <$> tSVal f x <*> tSVal f y

tSPairVal :: Traversal (SNum a) (SNum b) (SBaseNum a) (SBaseNum b)
tSPairVal f (SRegular x) = liftSNum <$> f (SBaseReg x)
tSPairVal f (SPair (SRegular x) (SRegular y)) = liftSNum <$> f (SBasePair x y)
tSPairVal f (SPair x y) =
  let xV = tSPairVal f x
      yV = tSPairVal f y
   in SPair <$> xV <*> yV

withDepth :: SNum a -> [(Int, SBaseNum a)]
withDepth (SRegular x) = [(0, SBaseReg x)]
withDepth (SPair (SRegular x) (SRegular y)) = [(0, SBasePair x y)]
withDepth (SPair x y) = (withDepth x <> withDepth y) & traversed . _1 %~ \x -> x + 1

itSPairVal :: IndexedFold Int (SNum a) (SBaseNum a)
itSPairVal = ifolding withDepth

-- | Traverse an SNum in terms of regular values
tSRegVal :: Traversal (SNum a) (SNum b) (SBaseNum a) (SBaseNum b)
tSRegVal f (SRegular x) = liftSNum <$> f (SBaseReg x)
tSRegVal f (SPair x y) = SPair <$> tSRegVal f x <*> tSRegVal f y

-- Operations

half :: Integral a => Bool -> a -> a
half u x =
  let res :: Double = fromIntegral x / 2
   in if u then ceiling res else floor res

sSplit :: Integral a => SNum a -> SNum a
sSplit (SRegular x) =
  if x < 10
    then SRegular x
    else SPair (SRegular (half False x)) (SRegular (half True x))
sSplit y = y

sBaseSplit :: Integral a => SBaseNum a -> SBaseNum a
sBaseSplit (SBaseReg x) =
  if x < 10
    then SBaseReg x
    else SBasePair (half False x) (half True x)
sBaseSplit y = y

data ReduceOp = Explode !Int | Split !Int | NoOp
  deriving (Show, Eq)

getExplodeOp :: SNum Int -> Maybe ReduceOp
getExplodeOp sn = do
  let xs = sn ^@.. itSPairVal
  i <- findIndexOf (itraversed <. filteredBy (_2 . _SBasePair) . _1) (>= 4) xs
  pure $ Explode i

getSplitOp :: SNum Int -> Maybe ReduceOp
getSplitOp sn = do
  let xs = sn ^.. tSRegVal
  i <- findIndexOf (itraversed <. _SBaseReg) (> 9) xs
  pure $ Split i

nextOp :: SNum Int -> ReduceOp
nextOp sn =
  head $
    catMaybes
      [ getExplodeOp sn,
        getSplitOp sn,
        Just NoOp
      ]

sNumSplitAt :: Int -> SNum Int -> SNum Int
sNumSplitAt i sn =
  let vals = sn ^.. tSRegVal
      vals' = vals & itraversed %@~ \j x -> if j == i then sBaseSplit x else x
   in sn & partsOf tSRegVal .~ vals'

addRight :: SBaseNum Int -> Int -> SBaseNum Int
addRight (SBaseReg x) t = SBaseReg $ x + t
addRight (SBasePair x y) t = SBasePair x (y + t)

addLeft :: SBaseNum Int -> Int -> SBaseNum Int
addLeft (SBaseReg x) t = SBaseReg $ x + t
addLeft (SBasePair x y) t = SBasePair (x + t) y

sNumExplodeAt :: Int -> SNum Int -> SNum Int
sNumExplodeAt i sn =
  let vals = sn ^.. tSPairVal
      -- v = preview (traversed . _SBasePair) $ take 1 . drop i $ vals
      v = vals ^? taking 1 (dropping i traversed) . _SBasePair
   in case v of
        Nothing -> sn
        Just (x, y) ->
          let vals' = vals & itraversed %@~ \j theVal ->
                if j == i -1 then addRight theVal x
                else if j == i + 1 then addLeft theVal y
                else if j == i then SBaseReg 0
                else theVal
           in sn & partsOf tSPairVal .~ vals'

execOp :: SNum Int -> ReduceOp -> SNum Int
execOp x NoOp = x
execOp x (Explode i) = sNumExplodeAt i x
execOp x (Split i) = sNumSplitAt i x

reduceSNum :: SNum Int -> IO (SNum Int)
reduceSNum x = case nextOp x of
  NoOp -> do
    -- putStrLn $ "[DEBUG] NOOP"
    pure x
  nOp -> do
    -- putStrLn $ "[DEBUG] nextOp: " <> show nOp
    let computed = execOp x nOp
    -- putStrLn $ "[DEBUG] computed: " <> printSNum computed
    if computed == x
      then do
        -- putStrLn $ "[DEBUG] Not updated"
        fail "Not update"
      else reduceSNum computed

sAdd :: SNum Int -> SNum Int -> IO (SNum Int)
sAdd x y = do
  -- putStrLn $ "[DEBUG] sAdd: " <> printSNum x <> " + " <> printSNum y
  let preReduced = SPair x y
  -- putStrLn $ "[DEBUG] to reduce: " <> printSNum preReduced
  reduceSNum $ preReduced

magnitude :: SNum Int -> Int
magnitude (SRegular x) = x
magnitude (SPair x y) = 3 * magnitude x + 2 * magnitude y

-- Parsers
sRegularParser :: Parser (SNum Int)
sRegularParser = do
  l <- many digitChar
  r :: Int <- embedMaybe . readMaybe $ l
  pure $ SRegular r

sNumParser :: Parser (SNum Int)
sNumParser = do
  _ <- char '['
  l <- try sRegularParser <|> sNumParser
  _ <- char ','
  r <- try sRegularParser <|> sNumParser
  _ <- char ']'
  pure (SPair l r)

-- Core

part1 :: [SNum Int] -> IO Int
part1 snums = do
  sumNum <- foldlM sAdd (head (take 1 snums)) (drop 1 snums)
  pure $ magnitude sumNum

part2 :: [SNum Int] -> IO Int
part2 snums = do
  let pairs = concat [[(x, y), (y, x)] | x <- snums, y <- snums, y /= x ]
  ms <- mapM (\(x, y) -> magnitude <$> sAdd x y) pairs
  pure $ maximum ms

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  snums <- parseList (embedMaybe . parseMaybe sNumParser) inputLines
  -- forM_ snums print
  res1 <- part1 snums
  putStrLn $ "A21E18 - part1: " <> show res1

  res2 <- part2 snums
  putStrLn $ "A21E18 - part2: " <> show res2