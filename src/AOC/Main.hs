module AOC.Main where

import qualified AOC.ES1 as ES1
import qualified AOC.ES2 as ES2
import qualified AOC.ES3 as ES3
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.App
import Lib.Exercise

-- Potentially can be Exercise -> Sem r ()
chooseAction :: Exercise -> IO ()
chooseAction (ES1 fp) = ES1.main fp
chooseAction (ES2 fp) = ES2.main fp
chooseAction (ES3 fp) = ES3.main fp
chooseAction None = liftIO $ putStrLn "Nothing to do"

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
    e <- grab @Exercise
    liftIO . putStrLn $ "Doing exercise " ++ show e
    liftIO $ chooseAction e