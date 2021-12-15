module AOC.A21.ES11 where

import AOC.A21.ES9 (lineParser)
import AOC.Utils
import AOC.Utils.Grid
import Control.Lens
import Control.Monad.State
import qualified Data.Foldable as F
import Text.Megaparsec

type AppMonad = StateT (Grid Int) IO

isFlashing :: Int -> Bool
isFlashing = (> 9)

flashIfNecessary :: Grid (Int, Bool) -> AppMonad (Grid (Int, Bool), Int)
flashIfNecessary grid = do
    -- liftIO . putStrLn $ "Got grid:\n" ++ showGrid grid ++ "\n"
    let -- position of pts > 9 that haven't flashed
        fPts =
            grid
                ^.. _withIndex
                    . filteredBy (_1 . _2 . filtered not)
                    . filteredBy (_1 . _1 . filtered isFlashing)
                    . _2
        flashedEls = F.foldl' (\acc cur -> acc & ix cur . _2 .~ True) grid fPts
        finalGrid = F.foldl' (\acc cur -> acc & _sqNbhd cur . _1 +~ 1) flashedEls fPts
    liftIO . putStrLn $ "Flashing points: " ++ show fPts
    pure (finalGrid, length fPts)

step :: AppMonad Int
step = do
    -- liftIO . putStrLn $ "Starting step"
    grid <- get
    let initGrid =
            grid
                & traversed %~ \x -> (x + 1, False)
        loop g = do
            res <- flashIfNecessary g
            case res of
                (finalGrid, 0) -> pure finalGrid
                (g', _) -> loop g'
    resGrid <- loop initGrid
    let newGrid =
            resGrid
                & traversed %~ fst
                & traversed . filtered isFlashing .~ 0
    put newGrid
    liftIO . putStrLn $ "After step:\n" ++ showGrid newGrid ++ "\n"
    pure $ lengthOf (traversed . filtered snd) resGrid

isSync :: Grid Int -> Bool
isSync = allOf traversed (== 0)

runSimulation :: Int -> AppMonad Int
runSimulation n = do
    grid <- get
    liftIO . putStrLn $ "Initial setup:\n" ++ showGrid grid ++ "\n"
    res <- replicateM n step
    pure $ sum res

findSync :: AppMonad Int
findSync = do
    let loop n = do
            g <- get
            if isSync g
                then pure n
                else step >> loop (n + 1)
    loop 0

part1 :: Grid Int -> IO Int
part1 grid = do
    (res, resultGrid) <- runStateT (runSimulation 100) grid
    pure res

part2 :: Grid Int -> IO Int
part2 grid = do
    evalStateT findSync grid

loadData :: FilePath -> IO (Grid Int)
loadData fp = do
    inputLines <- lines <$> readFile fp
    parsed <- mapM (embedMaybe . parseMaybe lineParser) inputLines
    createGrid parsed

main :: FilePath -> IO ()
main fp = do
    dataGrid <- loadData fp
    res1 <- part1 dataGrid
    putStrLn $ "Res1: " ++ show res1
    res2 <- part2 dataGrid
    putStrLn $ "Res2: " ++ show res2