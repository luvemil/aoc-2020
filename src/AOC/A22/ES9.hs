module AOC.A22.ES9 where

import AOC.Utils
import Control.Lens hiding ((<|))
import Control.Monad.State
import qualified Data.Foldable as F
import Data.List.NonEmpty as L
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

type Point = (Int, Int)

pDist :: Point -> Point -> Int
pDist (x, y) (x', y') = maximum [abs (x - x'), abs (y - y')]

pDist2 :: Point -> Point -> Int
pDist2 (x, y) (x', y') = abs (x - x') + abs (y - y')

data Mov = MovUp | MovDown | MovRight | MovLeft

type MyState =
  ( Point, -- head
    NonEmpty Point -- trail of the tail
  )

type Comp = StateT MyState Identity

movePt :: Mov -> Point -> Point
movePt MovUp (x, y) = (x, y + 1)
movePt MovDown (x, y) = (x, y - 1)
movePt MovRight (x, y) = (x + 1, y)
movePt MovLeft (x, y) = (x - 1, y)

runStep :: Mov -> Comp ()
runStep m = do
  (oldH, tTrail) <- get
  let newH = movePt m oldH
      oldT = L.head tTrail
  if pDist newH oldT > 1
    then put (newH, oldH <| tTrail)
    else put (newH, tTrail)

type MyState2 =
  ( NonEmpty Point, -- First 9 elements
    NonEmpty Point -- trail of the last element
  )

type Comp2 = StateT MyState2 Identity

newTailPos :: Point -> Point -> Point
newTailPos newH oldT =
  if pDist newH oldT <= 1
    then oldT
    else
      let (x, y) = oldT
       in F.minimumBy
            (comparing $ pDist2 newH)
            [ (x - a, y - b)
              | a <- [- 1, 0, 1],
                b <- [- 1, 0, 1],
                b /= 0 || a /= 0
            ]

runStep2 :: Mov -> Comp2 ()
runStep2 m = do
  (oldH :| oldTail, tTrail) <- get
  let newH = movePt m oldH
      oldT = L.head tTrail
      (newT, newTail) =
        F.foldl'
          ( \(newPrev, acc) cur ->
              if pDist newPrev cur > 1
                then
                  let newPos = newTailPos newPrev cur
                   in (newPos, acc <> [newPos])
                else (cur, acc <> [cur])
          )
          (newH, [])
          (oldTail <> [oldT])
  put (newH :| Prelude.init newTail, if newT /= oldT then newT <| tTrail else tTrail)

parseMov :: Parser (Mov, Int)
parseMov = do
  m <-
    choice
      [ MovUp <$ char 'U',
        MovDown <$ char 'D',
        MovRight <$ char 'R',
        MovLeft <$ char 'L'
      ]
  _ <- char ' '
  n <- embedMaybe . readMaybe =<< many digitChar
  pure (m, n)

--- Parts

part1 :: [(Mov, Int)] -> IO Int
part1 movs = do
  let action = forM_ movs $ \(m, n) -> replicateM_ n (runStep m)
      (_, tTrail) = execState action ((0, 0), (0, 0) :| [])
      uniqPos = uniq . L.toList $ tTrail
  pure $ Prelude.length uniqPos

part2 :: [(Mov, Int)] -> IO Int
part2 movs = do
  let action = forM_ movs $ \(m, n) -> replicateM_ n (runStep2 m)
      (_, tTrail) = execState action ((0, 0) :| [(0, 0) | _ <- [0 .. 7]], (0, 0) :| [])
      uniqPos = uniq . L.toList $ tTrail
  pure $ Prelude.length uniqPos

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  movs <- forM inputLines $ \l -> do
    case parse parseMov "Mov" l of
      Left peb -> fail $ errorBundlePretty peb
      Right x0 -> pure x0

  res1 <- part1 movs
  putStrLn $ "A22E09 - part1: " <> show res1

  res2 <- part2 movs
  putStrLn $ "A22E09 - part2: " <> show res2