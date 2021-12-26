module AOC.A21.ES15 where

import AOC.Utils
import AOC.Utils.Grid
import AOC.Utils.Tree
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State (StateT, execState, get, put)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Tree (Tree (Node))
import Text.Megaparsec (many, parseMaybe)
import Text.Megaparsec.Char (digitChar)
import Text.Read (readMaybe)

type AppMonad = ReaderT (Grid Int) Identity

type Path = [(Int, Int)]

getLastNode :: Path -> (Int, Int)
getLastNode path = fromMaybe startNode $ path ^? _last

startNode :: (Int, Int)
startNode = (0, 0)

endNode :: Grid a -> (Int, Int)
endNode (Grid w h _) = (w -1, h -1)

manhattanDist2 :: Num a => (a, a) -> (a, a) -> a
manhattanDist2 (x, y) (x', y') = abs (x - x') + abs (y - y')

isOnEdgeOf :: (Int, Int) -> Grid a -> Bool
(0, _) `isOnEdgeOf` _ = True
(_, 0) `isOnEdgeOf` _ = True
(x, y) `isOnEdgeOf` (Grid w h _) = x == w - 1 || y == h - 1

computeAvailable :: Grid a -> Path -> [(Int, Int)]
computeAvailable grid@(Grid w h _) path =
    let lastNode = getLastNode path
        prevNode =
            if length path > 1
                then path !? (length path - 2)
                else Nothing
        isNotPrev x = case prevNode of
            Nothing -> True
            Just x1 -> x /= x1
        adjacents' =
            grid ^@.. _inbhd lastNode
                & map fst
                & filter isNotPrev
        -- If the path is closing, it means that lastNode is adjacent to the path, so this
        -- branch can be discarded altogether
        adjacents =
            if any (`elem` path) adjacents'
                then []
                else adjacents'
     in -- If lastNode is on the edge of the map, only allow the point in the component
        -- connected to the end node
        if lastNode `isOnEdgeOf` grid
            then
                filter
                    ( \x ->
                        manhattanDist2 x (endNode grid) <= manhattanDist2 lastNode (endNode grid)
                    )
                    adjacents
            else adjacents

increasePath :: Path -> Int -> AppMonad (Maybe (Tree (Int, Int)))
increasePath path maxSize
    | length path > maxSize = pure Nothing
    | otherwise = do
        grid <- asks id
        let lastNode = getLastNode path
        if lastNode == endNode grid
            then pure . Just $ Node lastNode []
            else do
                let available = computeAvailable grid path
                    newPaths = [path ++ [x] | x <- available]
                inc <- forM newPaths $ \p -> increasePath p maxSize
                -- pure $ concat inc
                let remaining = catMaybes inc
                if null remaining
                    then pure Nothing
                    else pure . Just $ Node lastNode remaining

computeRiskValue :: Num a => Grid a -> Path -> a
computeRiskValue grid path = sumOf (itraversed . indices (`elem` path)) grid

type MinCompMonad = StateT (Maybe Int) Identity

stepMin :: Grid Int -> Path -> MinCompMonad [Maybe Int] -> MinCompMonad (Maybe Int)
stepMin grid path mxs = do
    cur <- get
    let riskVal = computeRiskValue grid path
    -- liftIO . putStrLn $ "Found riskVal: " ++ show riskVal ++ " for path: " ++ show path
    if maybe False (riskVal >=) cur
        then do
            -- Short circuit
            -- liftIO . putStrLn $ "Short circuiting path: " ++ show path
            pure Nothing
        else
            mxs >>= \case
                -- Leaf
                [] -> do
                    -- liftIO . putStrLn $ "Found min candidate path: " ++ show path
                    put $ Just riskVal
                    pure $ Just riskVal
                -- Path
                xs -> do
                    -- liftIO . putStrLn $ "Continue after path: " ++ show path
                    pure . Just . minimum . catMaybes $ xs

part1 :: Grid Int -> IO Int
part1 grid = do
    allPaths <- embedMaybe $ runReader (increasePath [] 10000) grid
    putStrLn $ "RunReader done"
    let expandedPaths = collect (:) [] allPaths
    let res = execState (foldTreeM (stepMin grid) expandedPaths) (Just 41)
    case res of
        Nothing -> error "Not Found"
        Just x -> pure x

parseRow :: Parser [Int]
parseRow = do
    digits <- many digitChar
    embedMaybe $ mapM (\c -> readMaybe [c]) digits

loadData :: FilePath -> IO (Grid Int)
loadData fp = do
    inputLines <- lines <$> readFile fp
    rows <- mapM (embedMaybe . parseMaybe parseRow) inputLines
    createGrid rows

main :: FilePath -> IO ()
main fp = do
    grid <- loadData fp
    res1 <- part1 grid
    putStrLn $ "Res1: " ++ show res1