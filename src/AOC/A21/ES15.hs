module AOC.A21.ES15 where

import AOC.Utils
import AOC.Utils.Grid
import AOC.Utils.Search
import AOC.Utils.Tree
import Control.Lens
import Control.Monad.Reader
import Control.Monad.State (StateT, execStateT, get, modify', put)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (Sum (..))
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

increasePath :: Path -> Int -> AppMonad (Maybe (Tree ((Int, Int), Path)))
increasePath path maxSize
    | length path > maxSize = pure Nothing
    | otherwise = do
        grid <- asks id
        let lastNode = getLastNode path
        if lastNode == endNode grid
            then pure . Just $ Node (lastNode, path) []
            else do
                let available = computeAvailable grid path
                    newPaths = [path ++ [x] | x <- available]
                inc <- forM newPaths $ \p -> increasePath p maxSize
                -- pure $ concat inc
                let remaining = catMaybes inc
                if null remaining
                    then pure Nothing
                    else pure . Just $ Node (lastNode, path) remaining

computeRiskValue :: Num a => Grid a -> Path -> a
computeRiskValue grid path = sumOf (itraversed . indices (`elem` path)) grid

computeRiskValueMemoized :: Path -> MinCompMonad Int
computeRiskValueMemoized path = do
    (_, cache) <- get
    case M.lookup (init path) cache of
        Nothing -> do
            grid <- ask
            let val = computeRiskValue grid path
            modify' $ \(x, _) -> (x, M.insert path val cache)
            pure val
        Just n -> do
            grid <- ask
            let val = n + fromMaybe 0 (grid ^? ix (last path))
            modify' $ \(x, _) -> (x, M.insert path val cache)
            pure val

type RiskValueCache = M.Map Path Int

type MinCompMonad = StateT (Maybe (Int, Path), RiskValueCache) (ReaderT (Grid Int) IO)

stepMin :: ((Int, Int), Path) -> MinCompMonad [Maybe Int] -> MinCompMonad (Maybe Int)
stepMin (_, path) mxs = do
    -- cur <- get >>= \s -> pure $ s >>= \x -> pure $ fst x
    cur <- get <&> fmap fst . fst
    grid <- ask
    -- let riskVal = computeRiskValue grid path
    riskVal <- computeRiskValueMemoized path
    let lastNode = getLastNode path
        heuristic = manhattanDist2 lastNode (endNode grid)
    liftIO . putStrLn $ "Found riskVal: " ++ show riskVal ++ " for path: " ++ show path
    if maybe False (riskVal - heuristic >=) cur
        then do
            -- Short circuit
            liftIO . putStrLn $ "Short circuiting path: " ++ show path
            pure Nothing
        else
            mxs >>= \case
                -- Leaf
                [] -> do
                    liftIO . putStrLn $ "Found min candidate path: " ++ show path
                    -- put $ Just (riskVal, path)
                    _ <- modify' $ \(_, x) -> (Just (riskVal, path), x)
                    pure $ Just riskVal
                -- Path
                xs -> do
                    liftIO . putStrLn $ "Continue after path: " ++ show path
                    pure . Just . minimum . catMaybes $ xs

part1 :: Grid Int -> IO Int
part1 grid = do
    allPaths <- embedMaybe $ runReader (increasePath [startNode] 10000) grid
    putStrLn $ "RunReader done"
    (res, _) <- runReaderT (execStateT (foldTreeM stepMin allPaths) (Nothing, M.empty)) grid
    case res of
        Nothing -> error "Not Found"
        Just (x, path) -> do
            putStrLn $ "Found min path: " ++ show path
            pure $ x - fromMaybe 0 (grid ^? ix (0, 0))

isGridAdjacent :: (Int, Int) -> (Int, Int) -> Bool
isGridAdjacent (x, y) (x', y') = abs (x - x') == 1 || abs (y - y') == 1

toPosMap :: Grid a -> M.Map (Int, Int) a
toPosMap grid = M.fromList $ grid ^@.. itraversed

getEdgeWeight :: Grid a -> (Int, Int) -> (Int, Int) -> ExtNum a
getEdgeWeight grid p q =
    if p `isGridAdjacent` q
        then maybe Infinity Finite $ grid ^? ix q
        else Infinity

getEdgeWeight' :: M.Map (Int, Int) a -> (Int, Int) -> (Int, Int) -> ExtNum a
getEdgeWeight' posMap p q =
    if p `isGridAdjacent` q
        then maybe Infinity Finite $ M.lookup p posMap
        else Infinity

part1' :: Grid Int -> IO Int
part1' grid = do
    let searchConfig =
            SearchConfig
                { scStartNode = startNode
                , scEndNode = endNode grid
                , scHeuristic = \x -> Finite $ manhattanDist2 x (endNode grid)
                , scGetNeighbors = \(x, y) -> getNeighborPos x y grid ^.. each . _Just
                , scEdgeWeight = getEdgeWeight grid
                }
        minWeightPath = reverse $ searchAStar searchConfig
    putStrLn $ "Found min path: " ++ show minWeightPath
    pure $ computeRiskValue grid (tail minWeightPath)

part2' :: Grid Int -> IO Int
part2' grid = do
    let posMap = grid `seq` toPosMap grid
        searchConfig =
            SearchConfig
                { scStartNode = startNode
                , scEndNode = endNode grid
                , scHeuristic = \x -> Finite $ manhattanDist2 x (endNode grid)
                , scGetNeighbors = \(x, y) -> getNeighborPos x y grid ^.. each . _Just
                , scEdgeWeight = getEdgeWeight' posMap
                }
        minWeightPath = reverse $ searchAStar searchConfig
    putStrLn $ "Found min path: " ++ show minWeightPath
    pure $ computeRiskValue grid (tail minWeightPath)

computeFullMap :: Grid Int -> Grid Int
computeFullMap grid@(Grid w h _) =
    let mgrid = grid & traversed %~ Sum
        gridH = mgrid `concatGridH` mgrid `concatGridH` mgrid `concatGridH` mgrid `concatGridH` mgrid
        gridV = gridH `concatGridV` gridH `concatGridV` gridH `concatGridV` gridH `concatGridV` gridH
     in gridV
            & traversed %~ getSum
            & itraversed
                %@~ ( \(x, y) a ->
                        a + (x `div` w) + (y `div` h)
                    )
            & traversed %~ \a -> if a > 9 then a - 9 else a

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
    res1 <- part1' grid
    putStrLn $ "Res1: " ++ show res1
    let fullGrid = computeFullMap grid & traversed %~ \x -> if x > 9 then x - 9 else x
    -- putStrLn $ showGrid fullGrid
    res2 <- part2' $ fullGrid
    putStrLn $ "Res2: " ++ show res2