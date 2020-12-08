{-# LANGUAGE TupleSections #-}

module AOC.ES8 (main) where

import AOC.Utils
import Control.Monad.State
import Data.Array
import Data.List (findIndices)
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Program = Array Int (Op, Int)

-- (accumulator, position, program)
type PState = (Int, Int, Program)

data Op
    = ACC Int
    | JMP Int
    | NOP Int
    deriving (Show, Eq)

type Parser = Parsec Void String

data Status = Running | Terminated | Loop
    deriving (Show, Eq)

opParser :: Parser Op
opParser = do
    opStr <- string "acc" <|> string "jmp" <|> string "nop"
    space
    sign :: Int <-
        choice
            [ 1 <$ char '+'
            , -1 <$ char '-'
            ]
    rest <- (* sign) . read <$> many printChar
    case opStr of
        "acc" -> pure $ ACC rest
        "jmp" -> pure $ JMP rest
        "nop" -> pure $ NOP rest
        _ -> fail "Cannot parse operation"

isTerminated :: State PState Bool
isTerminated = do
    (_, pos, program) <- get
    pure $ pos > (snd . bounds) program

checkStatus :: State PState Status
checkStatus = do
    s <- isTerminated
    if s
        then pure Terminated
        else pure Running

evalOp :: Op -> State PState Status
evalOp (ACC x) = do
    (acc, pos, program) <- get
    _ <- put (acc + x, pos + 1, program)
    checkStatus
evalOp (JMP x) = do
    (acc, pos, program) <- get
    _ <- put (acc, pos + x, program)
    checkStatus
evalOp (NOP _) = do
    (acc, pos, program) <- get
    _ <- put (acc, pos + 1, program)
    checkStatus

step :: State PState Status
step = do
    (_, pos, program) <- get
    let (op, _) = program ! pos
    evalOp op

getCounter :: State PState Int
getCounter = do
    (_, pos, program) <- get
    pure $ snd (program ! pos)

incCounter :: State PState Int
incCounter = do
    (acc, pos, program) <- get
    let (op, counter) = program ! pos
        newCounter = counter + 1
        newProg = program // [(pos, (op, newCounter))]
    _ <- put (acc, pos, newProg)
    pure newCounter

run1 :: State PState Int
run1 = do
    let loop = do
            c <- getCounter
            case c of
                0 -> incCounter >> step >> loop
                _ -> do
                    (acc, _, _) <- get
                    pure acc
    loop

run :: State PState (Int, Status)
run = do
    let loop = do
            c <- getCounter
            case c of
                0 ->
                    incCounter >> step >>= \case
                        Running -> loop
                        Terminated -> do
                            (acc, _, _) <- get
                            pure (acc, Terminated)
                        Loop -> undefined
                _ -> do
                    (acc, _, _) <- get
                    pure (acc, Loop)
    loop

buildProgram :: [Op] -> Program
buildProgram ops = listArray (0, length ops - 1) $ map (,0 :: Int) ops

switchAt :: Program -> Int -> Program
switchAt p i =
    let op = p ! i
        newEl = case op of
            (NOP x, j) -> (JMP x, j)
            (JMP x, j) -> (NOP x, j)
            _ -> undefined
     in p // [(i, newEl)]

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe opParser) inputLines
    let program = buildProgram parsed
        res = evalState run1 (0, 0, program)
    putStrLn $ "Part1 result: " ++ show res
    let matcher x = case x of
            ACC _ -> False
            _ -> True
        ixes = findIndices matcher parsed
        allPrograms = map (switchAt program) ixes
        results = map (evalState run . (0,0,)) allPrograms
        terminated = filter ((== Terminated) . snd) results
    putStrLn $ "Part2 result: " ++ show (fst . head $ terminated)