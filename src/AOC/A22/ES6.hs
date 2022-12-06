module AOC.A22.ES6 where

import AOC.Utils
import Control.Monad.State
import Data.List (elemIndex, findIndex)
import Data.Maybe

type MyState = (String, Int, [Char])

type Comp = StateT MyState

step :: Comp IO (Either () ())
step = do
  (s, pos, values) <- get
  let val' = s !? pos
  case val' of
    Nothing -> pure $ Left ()
    Just val ->
      case elemIndex val values of
        Nothing -> do 
            put (s, pos + 1, values <> [val])
            pure $ Right ()
        Just n -> do
            put (s, pos + 1, drop (n + 1) values <> [val])
            pure $ Right ()

isDone :: Int -> Comp IO (Maybe Int)
isDone n = do
  (_, pos, values) <- get
  if length values >= n
    then pure $ Just pos
    else pure Nothing

parseSignal :: Int -> Comp IO Int
parseSignal size = do
  res <- isDone size
  case res of
    Nothing -> do
      res' <- step
      case res' of
        Left _ -> fail "End of input"
        Right _ -> parseSignal size
    Just n -> pure n

part1 :: String -> IO Int
part1 s = do
    evalStateT (parseSignal 4) (s, 0, [])

part2 :: String -> IO Int
part2 s = do
    evalStateT (parseSignal 14) (s, 0, [])

main :: FilePath -> IO ()
main fp = do
  input <- readFile fp

  res1 <- part1 input
  putStrLn $ "A22E06 - part1: " <> show res1

  res2 <- part2 input
  putStrLn $ "A22E06 - part2: " <> show res2