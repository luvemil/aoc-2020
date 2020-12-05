{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AOC.Main where

import qualified AOC.ES1 as ES1
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.App
import Options.Generic

data Exercise = ES1
    deriving (Generic, Show)

instance ParseRecord Exercise

chooseAction :: Exercise -> App ()
chooseAction ES1 = ES1.main

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
    e <- getRecord "Exercise"
    liftIO . putStrLn $ "Doing exercise " ++ show e
    chooseAction e