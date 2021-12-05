{-# LANGUAGE TupleSections #-}

module AOC.ES11 (main) where

import AOC.Utils
import Control.Monad (join)
import Control.Monad.State
import Data.Array
import Data.Function ((&))
import Data.Void (Void)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char

type Coords = (Int, Int)

data SeatState = Empty | Full | Floor
    deriving (Eq, Show)

type SeatsMapT a = Array Coords a
type SeatsMap = SeatsMapT (SeatState, StepState)

seatParser :: Parser SeatState
seatParser =
    choice
        [ Empty <$ char 'L'
        , Full <$ char '#'
        , Floor <$ char '.'
        ]

seatLineParser :: Parser [SeatState]
seatLineParser = many seatParser

buildMap :: [[SeatState]] -> SeatsMap
buildMap parsed =
    let w = length $ head parsed
        h = length parsed
        p = map (,Running) $ join parsed
     in listArray ((1, 1), (w, h)) p

type SeatsProg = State SeatsMap

computeNewState :: Coords -> SeatsMap -> (SeatState, StepState)
computeNewState (x, y) seatsMap =
    let adjacents =
            [ (x + i, y + j)
            | i <- [-1, 0, 1]
            , j <- [-1, 0, 1]
            ]
                & filter (/= (x, y))
                & filter inBounds
                & map (fst . (seatsMap !))
        inBounds = inRange (bounds seatsMap)
        -- inBounds (s, t) =
        --     let ((a, b), (a', b')) = bounds seatsMap
        --      in s >= a && s <= a' && t >= b && t <= b'
        cur = seatsMap ! (x, y)
        allEmpty = Full `notElem` adjacents
        numFull = length $ filter (== Full) adjacents
     in case cur of
            (Floor, _) -> (Floor, Done)
            (cur', _) ->
                if cur' == Empty && allEmpty
                    then (Full, Running)
                    else
                        if cur' == Full && numFull >= 4
                            then (Empty, Running)
                            else (cur', Done)

data StepState = Done | Running
    deriving (Eq, Show)

step :: SeatsProg StepState
step = do
    oldMap <- get
    let ixes = indices oldMap
        f ix =
            let newState = computeNewState ix oldMap
             in newState `seq` (ix, newState)
        newList =
            ixes
                & filter ((/= Done) . snd . (oldMap !)) -- only check those that changed
                & map f
                & filter ((/= Done) . snd . snd)
        -- newMap :: SeatsMap = newList `seq` array (bounds oldMap) newList
        newMap :: SeatsMap = oldMap // newList
    if null newList
        then pure Done
        else put newMap >> pure Running

run1 :: SeatsProg SeatsMap
run1 = do
    let loop =
            step >>= \case
                Running -> loop
                Done -> get
    loop

runLimit :: Int -> SeatsProg StepState
runLimit 0 = pure Running
runLimit i =
    step >>= \case
        Running -> runLimit $ i - 1
        Done -> pure Done

showSeatsLine :: [SeatState] -> String
showSeatsLine = map $ \case
    Empty -> 'L'
    Full -> '#'
    Floor -> '.'

splitLines :: SeatsMap -> [[SeatState]]
splitLines seatsMap =
    let rows =
            seatsMap
                & indices
                & map snd
                & uniq
        allSeats = assocs seatsMap
        getSeatsLine i =
            allSeats
                & filter ((== i) . snd . fst)
                & map (fst . snd)
     in map getSeatsLine rows

printSeatsLines :: [[SeatState]] -> IO ()
printSeatsLines [] = pure ()
printSeatsLines (x : xs) = do
    putStrLn $ showSeatsLine x
    printSeatsLines xs

printSeatsMap :: SeatsMap -> IO ()
printSeatsMap = printSeatsLines . splitLines

updateMap :: SeatsMap -> SeatsMap
updateMap oldMap =
    let ixes = indices oldMap
        f ix =
            let newState = computeNewState ix oldMap
             in newState `seq` (ix, newState)
        newList =
            ixes
                & filter ((/= Done) . snd . (oldMap !)) -- only check those that changed
                & map f
                & filter ((/= Done) . snd . snd)
     in -- newMap :: SeatsMap = newList `seq` array (bounds oldMap) newList
        oldMap // newList

main :: FilePath -> Int -> IO ()
main fp myLim = do
    inputLines <- lines <$> readFile fp
    parsed <- parseList (embedMaybe . parseMaybe seatLineParser) inputLines
    let myElems = length . filter (== Full) . map fst . elems
        seatsMap = buildMap parsed
        state1 = evalState run1 seatsMap
        res1 = myElems state1
    -- putStrLn $ "Part 1 result: " ++ show res1
    let (s, state2) = runState (runLimit myLim) seatsMap
        res2 =
            elems state2
                & map fst
                & filter (== Full)
                & length
    -- putStrLn $ "After " ++ show myLim ++ " state is " ++ show s ++ " res is " ++ show res2
    -- let state' = execState (step >> step >> step >> step >> step >> step) seatsMap
    -- putStrLn "-----\n-----\n-----\nFirst lines:"
    -- printSeatsLines . take 3 . splitLines $ state2
    let state3 = findFixedPointIter (==) updateMap seatsMap
        res3 = myElems state3

    putStrLn $ "Part1 result: " ++ show res3