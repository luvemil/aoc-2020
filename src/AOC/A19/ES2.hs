module AOC.A19.ES2 (main) where

import AOC.Utils
import Control.Monad.State
import Data.Array

type Program = Array Pos Val
type Pos = Int
type Val = Int

type PState = (Pos, Program)

putVal :: Pos -> Val -> State PState ()
putVal i x = do
    (pos, program) <- get
    let newProg = program // [(i, x)]
    put (pos, newProg)
    pure ()

movePos :: Pos -> State PState ()
movePos i = do
    (pos, program) <- get
    let newPos = pos + i
    put (newPos, program)
    pure ()

data Status = Running | Finished | Error
    deriving (Show, Eq)

getArgs :: State PState (Val, Val, Pos)
getArgs = do
    (pos, program) <- get
    let pos1 = program ! (pos + 1)
        pos2 = program ! (pos + 2)
        pos3 = program ! (pos + 3)
    pure (program ! (pos1 + 1), program ! (pos2 + 1), pos3 + 1)

step :: State PState Status
step = do
    (pos, program) <- get
    let code = program ! pos
    case code of
        1 -> do
            -- OPCODE ADD
            (x, y, i) <- getArgs
            putVal i (x + y)
            movePos 4
            pure Running
        2 -> do
            -- OPCODE MULTIPLY
            (x, y, i) <- getArgs
            putVal i (x * y)
            movePos 4
            pure Running
        99 -> pure Finished
        _ -> pure Error

runProgram :: State PState Status
runProgram = do
    let loop =
            step >>= \case
                Running -> loop
                x -> pure x
    loop

fullProgram :: Int -> Int -> State PState Val
fullProgram noun verb = do
    putVal 2 noun
    putVal 3 verb
    _ <- runProgram
    (_, s) <- get
    pure $ s ! 1

getOutput :: Program -> Int -> Int -> Val
getOutput p n v = evalState (fullProgram n v) s
  where
    s = (1, p)

testProgram :: [Int]
testProgram = [1, 9, 10, 3, 2, 3, 11, 0, 99, 30, 40, 50]

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    parsed :: [Val] <- parseList readIO $ splitOn ',' (head inputLines)
    let tProg = listArray (1, length testProgram) testProgram
        (res, (pos, tRes)) = runState (step >> step >> step) (1 :: Int, tProg)
    putStrLn "Running tests"
    print res
    print (pos, elems tRes)
    let program = listArray (1, length parsed) parsed
        fullAction = do
            putVal 2 12
            putVal 3 2
            runProgram
        (result, (_, output)) = runState fullAction (1 :: Int, program)
    putStrLn $ "Part1 Result: " ++ show result
    putStrLn $ "Part1 Output: " ++ show (elems output)
    putStrLn $ "Part2 test: " ++ show (getOutput program 12 2)
    let domain = [(i, j) | i <- [0 .. 99], j <- [0 .. 99]] :: [(Int, Int)]
        toOutput (x, y) = getOutput program x y
        finalOutput = filter ((== 19690720) . toOutput) domain
    case finalOutput of
        [(x, y)] -> putStrLn $ "Part2 Result: " ++ show (x, y) ++ " -> " ++ show (100 * x + y)
        _ -> putStrLn "No valid output found"