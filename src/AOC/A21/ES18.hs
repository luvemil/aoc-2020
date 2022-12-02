module AOC.A21.ES18 where
import Control.Lens

data SNum a = SRegular !a | SPair !(SNum a) !(SNum a)
  deriving (Show, Eq)

tSVal :: Traversal (SNum a) (SNum b) a b -- Applicative f => (a -> f b) -> (SNum a) -> f (SNum b)
tSVal f (SRegular x) = SRegular <$> f x
tSVal f (SPair x y) = SPair <$> tSVal f x <*> tSVal f y

data SPos = L | R
  deriving (Show, Eq)

tSValIx :: IndexedTraversal [SPos] (SNum a) (SNum b) a b
tSValIx p (SRegular x) = SRegular <$> indexed p [] x
tSValIx p (SPair x y) = _g

sAdd :: SNum a -> SNum a -> SNum a
sAdd = SPair

-- sReduce :: Num a => SNum a -> SNum a
-- sReduce (SRegular x) = SRegular x
-- sReduce _ = _f

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

main :: FilePath -> IO ()
main fp = do
  input <- readFile fp
  putStrLn input