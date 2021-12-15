{-# LANGUAGE TemplateHaskell #-}

module AOC.A21.ES12 where

import AOC.Utils
import Control.Lens
import Control.Monad.Reader
import Data.Char (toLower, toUpper)
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char

data MyNode = Small String | Big String
    deriving (Show, Eq, Ord)

makePrisms ''MyNode

_val :: Traversal' MyNode String
_val handler (Small x) = Small <$> handler x
_val handler (Big x) = Small <$> handler x

data Segment = Segment MyNode MyNode
    deriving (Show, Eq, Ord)

flipSegment :: Segment -> Segment
flipSegment (Segment x y) = Segment y x

type Path = [Segment]

-- (all segments, start node, end node)
type AppMonad = ReaderT ([Segment], MyNode, MyNode) IO

_pair :: Iso' Segment (MyNode, MyNode)
_pair = iso to' from'
  where
    to' (Segment x y) = (x, y)
    from' (x, y) = Segment x y

startingWith :: MyNode -> [Segment] -> [Segment]
startingWith node available = available ^.. traverse . filteredBy (_pair . _1 . filtered (== node))

increasePath :: Path -> Int -> AppMonad [Path]
increasePath path maxSize
    | length path > maxSize = pure []
    | otherwise = do
        (available, startNode, endNode) <- asks id
        let lastNode = fromMaybe startNode $ path ^? _last . _pair . _2
        if lastNode == endNode
            then pure [path]
            else do
                let candidates = startingWith lastNode available
                    smallDone = path ^.. traversed . _pair . each . filteredBy _Small
                    allowed = candidates ^.. traversed . filteredBy (_pair . _2 . filtered (`notElem` smallDone))
                    newPaths = [path ++ [x] | x <- allowed]
                inc <- forM newPaths $ \p -> increasePath p maxSize
                pure $ concat inc

part1 :: [Segment] -> IO Int
part1 allSegments = do
    startNode <- embedMaybe $ allSegments ^? traversed . _pair . each . filteredBy (_val . filtered (== "start"))
    endNode <- embedMaybe $ allSegments ^? traversed . _pair . each . filteredBy (_val . filtered (== "end"))
    let available = allSegments ++ map flipSegment allSegments
    allPaths <- runReaderT (increasePath [] 40) (available, startNode, endNode)
    pure $ length allPaths

increasePath2 :: Path -> Int -> AppMonad [Path]
increasePath2 path maxSize
    | length path > maxSize = pure []
    | otherwise = do
        (available, startNode, endNode) <- asks id
        let lastNode = fromMaybe startNode $ path ^? _last . _pair . _2
        if lastNode == endNode
            then pure [path]
            else do
                let candidates = startingWith lastNode available
                    smallDone' = path ^.. traversed . _pair . _2 . filteredBy _Small
                    smallDone'' =
                        if not (null path)
                            then startNode : smallDone'
                            else smallDone'
                    smallDoneUnique = uniq smallDone''
                    smallDone =
                        if length smallDone'' == length smallDoneUnique
                            then smallDone'' ^.. traversed . filtered (== startNode)
                            else smallDone''
                    allowed = candidates ^.. traversed . filteredBy (_pair . _2 . filtered (`notElem` smallDone))
                    newPaths = [path ++ [x] | x <- allowed]
                -- liftIO . putStrLn $ "smallDone': " ++ show smallDone'
                -- liftIO . putStrLn $ "smallDone: " ++ show smallDone
                -- liftIO . putStrLn $ "allowed: " ++ show allowed
                -- liftIO . putStrLn $ "\n"
                inc <- forM newPaths $ \p -> increasePath2 p maxSize
                pure $ concat inc

part2 :: [Segment] -> IO Int
part2 allSegments = do
    startNode <- embedMaybe $ allSegments ^? traversed . _pair . each . filteredBy (_val . filtered (== "start"))
    endNode <- embedMaybe $ allSegments ^? traversed . _pair . each . filteredBy (_val . filtered (== "end"))
    let available = allSegments ++ map flipSegment allSegments
    allPaths <- runReaderT (increasePath2 [] 60) (available, startNode, endNode)
    pure $ length allPaths

toNode :: MonadFail m => String -> m MyNode
toNode s
    | s == map toUpper s = pure $ Big s
    | s == map toLower s = pure $ Small s
    | otherwise = fail "Expecting same case string"

segmentParser :: Parser Segment
segmentParser = do
    startVal <- toNode =<< many alphaNumChar <* char '-'
    endVal <- toNode =<< many alphaNumChar
    pure $ Segment startVal endVal

main :: FilePath -> IO ()
main fp = do
    inputLines <- lines <$> readFile fp
    segments <- mapM (embedMaybe . parseMaybe segmentParser) inputLines
    res <- part1 segments
    putStrLn $ "Res1: " ++ show res
    res2 <- part2 segments
    putStrLn $ "Res2: " ++ show res2