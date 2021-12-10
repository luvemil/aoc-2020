module AOC.Main where

import qualified AOC.A19.ES1 as A19E1
import qualified AOC.A19.ES2 as A19E2
import qualified AOC.A19.ES3 as A19E3
import qualified AOC.A19.ES4 as A19E4
import qualified AOC.A21.ES1 as A21E1
import qualified AOC.A21.ES2 as A21E2
import qualified AOC.A21.ES3 as A21E3
import qualified AOC.A21.ES4 as A21E4
import qualified AOC.A21.ES5 as A21E5
import qualified AOC.A21.ES6 as A21E6
import qualified AOC.A21.ES7 as A21E7
import qualified AOC.A21.ES8 as A21E8
import qualified AOC.A21.ES9 as A21E9
import qualified AOC.ES1 as ES1
import qualified AOC.ES10 as ES10
import qualified AOC.ES11 as ES11
import qualified AOC.ES2 as ES2
import qualified AOC.ES3 as ES3
import qualified AOC.ES4 as ES4
import qualified AOC.ES5 as ES5
import qualified AOC.ES6 as ES6
import qualified AOC.ES7 as ES7
import qualified AOC.ES8 as ES8
import qualified AOC.ES9 as ES9
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
chooseAction (ES7 fp) = ES7.main fp
chooseAction (ES8 fp) = ES8.main fp
chooseAction (ES9 fp) = ES9.main fp
chooseAction (ES10 fp) = ES10.main fp
chooseAction (ES11 fp x) = ES11.main fp x
chooseAction (A19E1 fp) = A19E1.main fp
chooseAction (A19E2 fp) = A19E2.main fp
chooseAction (A19E3 fp) = A19E3.main fp
chooseAction (A19E4 fp) = A19E4.main fp
chooseAction (A21E1 fp) = A21E1.main fp
chooseAction (A21E2 fp) = A21E2.main fp
chooseAction (A21E3 fp) = A21E3.main fp
chooseAction (A21E4 fp) = A21E4.main fp
chooseAction (A21E5 fp) = A21E5.main fp
chooseAction (A21E6 fp x) = A21E6.main fp x
chooseAction (A21E7 fp) = A21E7.main fp
chooseAction (A21E8 fp) = A21E8.main fp
chooseAction (A21E9 fp) = A21E9.main fp
chooseAction None = liftIO $ putStrLn "Nothing to do"

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
    e <- grab @Exercise
    liftIO . putStrLn $ "Doing exercise " ++ show e
    liftIO $ chooseAction e