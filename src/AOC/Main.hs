module AOC.Main where

import qualified AOC.ES1 as ES1
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.App
import Lib.Exercise

-- Potentially can be Exercise -> Sem r ()
chooseAction :: Exercise -> IO ()
chooseAction (ES1 fn) = ES1.main fn
chooseAction None = liftIO $ putStrLn "Nothing to do"

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
    e <- grab @Exercise
    liftIO . putStrLn $ "Doing exercise " ++ show e
    liftIO $ chooseAction e