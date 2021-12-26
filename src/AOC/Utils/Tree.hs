module AOC.Utils.Tree where

import Data.Tree

foldTreeA :: Applicative f => (a -> f [b] -> f b) -> Tree a -> f b
foldTreeA h (Node x xs) = h x (traverse (foldTreeA h) xs)

foldTreeM :: Monad m => (a -> m [b] -> m b) -> Tree a -> m b
foldTreeM f (Node x xs) = f x (mapM (foldTreeM f) xs)

collect :: (a -> b -> b) -> b -> Tree a -> Tree b
collect f v (Node x xs) = Node (f x v) (map (collect f (f x v)) xs)