module AOC.Main where

import qualified AOC.ES1 as ES1
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.App
import Lib.Exercise

chooseAction :: Exercise -> App ()
chooseAction ES1 = ES1.main
chooseAction None = liftIO $ putStrLn "Nothing to do"

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
    e <- grab @Exercise
    liftIO . putStrLn $ "Doing exercise " ++ show e
    chooseAction e