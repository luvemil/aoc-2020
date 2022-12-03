module AOC.A22.ES2 (main) where

import AOC.Utils (Parser, embedMaybe, parseList)
import Control.Lens ((^?!))
import Control.Lens.Combinators
import Text.Megaparsec (choice, parseMaybe)
import Text.Megaparsec.Char (char, space)

data RPSAction = Rock | Paper | Scissor
  deriving (Show, Eq)

actionVal :: RPSAction -> Int
actionVal Rock = 1
actionVal Paper = 2
actionVal Scissor = 3

opponentActionParser :: Parser RPSAction
opponentActionParser =
  choice
    [ Rock <$ char 'A',
      Paper <$ char 'B',
      Scissor <$ char 'C'
    ]

playerActionParser :: Parser RPSAction
playerActionParser =
  choice
    [ Rock <$ char 'X',
      Paper <$ char 'Y',
      Scissor <$ char 'Z'
    ]

data RPSRound a = RPSRound
  { opMove :: RPSAction,
    plMove :: a
  }
  deriving (Show, Eq)

data RPSResult = OPWins | PLWins | Draw
  deriving (Show, Eq)

resultParser :: Parser RPSResult
resultParser =
  choice
    [ OPWins <$ char 'X',
      Draw <$ char 'Y',
      PLWins <$ char 'Z'
    ]

roundResult :: RPSRound RPSAction -> RPSResult
roundResult (RPSRound Rock Paper) = PLWins
roundResult (RPSRound Paper Scissor) = PLWins
roundResult (RPSRound Scissor Rock) = PLWins
roundResult (RPSRound Rock Scissor) = OPWins
roundResult (RPSRound Paper Rock) = OPWins
roundResult (RPSRound Scissor Paper) = OPWins
roundResult _ = Draw

roundVal :: RPSRound RPSAction -> Int
roundVal r =
  let plVal = actionVal $ plMove r
      rVal = case roundResult r of
        PLWins -> 6
        Draw -> 3
        OPWins -> 0
   in plVal + rVal

roundParser :: Parser (RPSRound RPSAction)
roundParser =
  RPSRound <$> (opponentActionParser <* space) <*> playerActionParser

roundP2Parser :: Parser (RPSRound RPSResult)
roundP2Parser =
  RPSRound <$> (opponentActionParser <* space) <*> resultParser

part1 :: [RPSRound RPSAction] -> IO Int
part1 game = do
  let r = sum $ map roundVal game
  pure r

convertRound :: RPSRound RPSResult -> RPSRound RPSAction
convertRound (RPSRound x Draw) = RPSRound x x
convertRound (RPSRound Paper OPWins) = RPSRound Paper Rock
convertRound (RPSRound Scissor PLWins) = RPSRound Scissor Rock
convertRound (RPSRound Scissor OPWins) = RPSRound Scissor Paper
convertRound (RPSRound Rock PLWins) = RPSRound Rock Paper
convertRound (RPSRound x _) = RPSRound x Scissor

-- convertRound :: RPSRound RPSResult -> RPSRound RPSAction
-- convertRound (RPSRound op res) =
--   let allVals = [Rock, Paper, Scissor]
--       allRes = map (roundResult . RPSRound op) allVals
--       valRes = zip allVals allRes
--       good = filter ((== res) . snd) valRes
--       plAction = good ^?! traversed
--    in RPSRound op (fst plAction)

part2 :: [RPSRound RPSResult] -> IO Int
part2 game = do
  let game' = map convertRound game
  part1 game'

main :: FilePath -> IO ()
main fp = do
  inputLines <- lines <$> readFile fp
  gameRounds <- embedMaybe . parseList (parseMaybe roundParser) $ inputLines

  res1 <- part1 gameRounds

  putStrLn $ "A22E2 - part1: " <> show res1

  game2Rounds <- embedMaybe . parseList (parseMaybe roundP2Parser) $ inputLines

  res2 <- part2 game2Rounds

  putStrLn $ "A22E2 - part2: " <> show res2
