module AOC.Main where

import qualified AOC.A19.ES1 as A19E1
import qualified AOC.ES1 as ES1
import qualified AOC.ES2 as ES2
import qualified AOC.ES3 as ES3
import qualified AOC.ES4 as ES4
import qualified AOC.ES5 as ES5
import qualified AOC.ES6 as ES6
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.App
import Lib.Exercise

-- Potentially can be Exercise -> Sem r ()
chooseAction :: Exercise -> IO ()
chooseAction (ES1 fp) = ES1.main fp
chooseAction (ES2 fp) = ES2.main fp
chooseAction (ES3 fp) = ES3.main fp
chooseAction (ES4 fp) = ES4.main fp
chooseAction (ES5 fp) = ES5.main fp
chooseAction (ES6 fp) = ES6.main fp
chooseAction (A19E1 fp) = A19E1.main fp
chooseAction None = liftIO $ putStrLn "Nothing to do"

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
    e <- grab @Exercise
    liftIO . putStrLn $ "Doing exercise " ++ show e
    liftIO $ chooseAction e