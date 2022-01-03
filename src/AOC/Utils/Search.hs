{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

module AOC.Utils.Search where

import Control.Lens
import Control.Monad.Reader
import Control.Monad.State
import Data.Generics.Labels
import Data.Generics.Product
import Data.Generics.Sum
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ord
import Data.Set as S
import GHC.Generics (Generic)

data SearchConfig p a = SearchConfig
    { scStartNode :: p
    , scEndNode :: p
    , scHeuristic :: p -> a
    , scGetNeighbors :: p -> [p]
    , scEdgeWeight :: p -> p -> a -- current -> neighbor -> dist
    }

data SearchState a p = SearchState
    { openSet :: S.Set p
    , comeFrom :: M.Map p p
    , gScore :: M.Map p a
    , fScore :: M.Map p a
    }
    deriving (Generic, Eq)

type SearchMonad a p = ReaderT (SearchConfig p a) (State (SearchState a p))

toGetter :: Ord a => M.Map a b -> a -> Maybe b
toGetter = flip M.lookup

toGetterDefault :: Ord a => b -> M.Map a b -> a -> b
toGetterDefault b m = fromMaybe b . toGetter m

data ExtNum a where
    Infinity :: ExtNum a
    Finite :: a -> ExtNum a

addExtNum :: Num a => ExtNum a -> ExtNum a -> ExtNum a
addExtNum Infinity _ = Infinity
addExtNum _ Infinity = Infinity
addExtNum (Finite x) (Finite y) = Finite $ x + y

deriving instance Eq a => Eq (ExtNum a)

instance Ord a => Ord (ExtNum a) where
    _ <= Infinity = True
    Infinity <= _ = False
    Finite x <= Finite y = x <= y

reconstructPath :: Ord p => M.Map p p -> p -> [p]
reconstructPath m cur = cur : rest
  where
    rest = case M.lookup cur m of
        Nothing -> []
        Just p -> reconstructPath m p

removeFromOpenSet :: Ord p => p -> SearchMonad a p ()
removeFromOpenSet current = modify . over #openSet $ S.delete current

computeTentativeGScore :: (Num a, Ord p) => p -> p -> SearchMonad (ExtNum a) p (ExtNum a)
computeTentativeGScore current neighbor = do
    SearchState{..} <- get
    SearchConfig{..} <- ask
    let curGScore = toGetterDefault Infinity gScore current
        dist = scEdgeWeight current neighbor
    pure $ curGScore `addExtNum` dist

updateNeighbor :: (Ord p, Ord a, Num a) => p -> p -> SearchMonad (ExtNum a) p ()
updateNeighbor current p = do
    SearchState{..} <- get
    SearchConfig{..} <- ask
    tentative_gScore <- computeTentativeGScore current p
    let neighbor_gScore = toGetterDefault Infinity gScore p
    if tentative_gScore < neighbor_gScore
        then do
            modify . over #comeFrom $ M.insert p current
            modify . over #gScore $ M.insert p tentative_gScore
            modify . over #fScore $ M.insert p $ addExtNum tentative_gScore (scHeuristic p)
            modify . over #openSet $ S.insert p
        else pure ()

-- TODO: add short circuit monad
step :: (Ord p, Ord a, Num a) => SearchMonad (ExtNum a) p (Either [p] (S.Set p))
step = do
    SearchState{..} <- get
    SearchConfig{..} <- ask
    let current = minimumByOf folded (comparing (toGetterDefault Infinity fScore)) openSet
    if current == Just scEndNode
        then pure . Left $ reconstructPath comeFrom scEndNode -- TODO: edit this return?
        else do
            case current of
                Nothing -> pure . Right $ S.empty
                Just p -> do
                    removeFromOpenSet p
                    mapM_ (updateNeighbor p) $ scGetNeighbors p
                    gets $ Right . view #openSet

search :: (Ord p, Num a, Ord a) => SearchMonad (ExtNum a) p [p]
search = do
    let loop =
            step >>= \case
                Left ps -> pure ps
                Right res ->
                    if S.null res
                        then pure []
                        else loop
    loop

searchAStar :: (Ord p, Ord a, Num a) => SearchConfig p (ExtNum a) -> [p]
searchAStar sc@SearchConfig{..} =
    let openSet = S.singleton scStartNode
        comeFrom = M.empty
        gScore = M.fromList [(scStartNode, Finite 0)]
        fScore = M.fromList [(scStartNode, scHeuristic scStartNode)]
        initialSearchState = SearchState{..}
     in evalState (runReaderT search sc) initialSearchState