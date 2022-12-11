module AOC.A22.ES10 where

import AOC.Utils
import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Read (readMaybe)

data Command = Noop | AddX !Integer
  deriving (Show, Eq)

type PCState =
  ( Int, -- cycle count
    Integer -- register value
  )

type Comp = StateT PCState (WriterT [(Int, Integer)] (ReaderT [Int] Identity))

nextCycle :: Comp ()
nextCycle = do
  modify' $ over _1 (+ 1)
  (cycleCount, val) <- get
  outCycles <- ask
  if cycleCount `elem` outCycles
    then do
      tell [(cycleCount, val)]
      pure ()
    else pure ()

runCommand :: Command -> Comp ()
runCommand Noop = nextCycle
runCommand (AddX v) = do
  nextCycle
  nextCycle
  modify' $ over _2 (+ v)

type Comp2 = StateT PCState (WriterT String Identity)

nextCycle2 :: Comp2 ()
nextCycle2 = do
  (cycleCount, val) <- get
  let pos = cycleCount `mod` 40
  if cycleCount > 0 && pos == 0
    then tell $ '\n' : []
    else pure ()
  if abs (fromIntegral pos - val) <= 1
    then tell "#"
    else tell "."
  modify' $ over _1 (+ 1)

runCommand2 :: Command -> Comp2 ()
runCommand2 Noop = nextCycle2
runCommand2 (AddX v) = do
  nextCycle2
  nextCycle2
  modify' $ over _2 (+ v)


-- Parsers

addXParser :: Parser Command
addXParser = do
  _ <- string "addx "
  sign :: Integer <- (try (char '-') $> (-1)) <|> pure 1
  val <- embedMaybe . readMaybe =<< many digitChar
  pure . AddX $ sign * val

parseCommand :: Parser Command
parseCommand = do
  choice
    [ Noop <$ string "noop",
      addXParser
    ]

-- Parts

runProgram1 :: [Int] -> Comp () -> [(Int, Integer)]
runProgram1 outCycles p =
  p
    & flip evalStateT (0, 1)
    & execWriterT
    & flip runReader outCycles

part1 :: [Command] -> IO Integer
part1 commands = do
  let outCycles :: [Int] = [20, 60, 100, 140, 180, 220]
      program = forM_ commands runCommand
      res = runProgram1 outCycles program
      res' = sum $ map (\(c, v) -> fromIntegral c * v) res
  pure res'


runProgram2 :: Comp2 () -> String
runProgram2 p =
  p
    & flip evalStateT (0, 1)
    & execWriter

part2 :: [Command] -> IO String
part2 commands = do
  let program = forM_ commands runCommand2
      res = runProgram2 program
  pure res


main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  commands <- forM inputLines $ \l ->
    case parse parseCommand "Command" l of
      Left peb -> fail $ errorBundlePretty peb
      Right com -> pure com

  -- forM_ commands print
  res1 <- part1 commands
  putStrLn $ "A22E10 - part1: " <> show res1

  res2 <- part2 commands
  putStrLn $ "A22E10 - part2:"
  forM_ (splitOn '\n' res2) putStrLn
