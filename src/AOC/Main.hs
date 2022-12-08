module AOC.Main where

import qualified AOC.A19.ES1 as A19E1
import qualified AOC.A19.ES2 as A19E2
import qualified AOC.A19.ES3 as A19E3
import qualified AOC.A19.ES4 as A19E4
import qualified AOC.A21.ES1 as A21E1
import qualified AOC.A21.ES10 as A21E10
import qualified AOC.A21.ES11 as A21E11
import qualified AOC.A21.ES12 as A21E12
import qualified AOC.A21.ES13 as A21E13
import qualified AOC.A21.ES14 as A21E14
import qualified AOC.A21.ES15 as A21E15
import qualified AOC.A21.ES16 as A21E16
import qualified AOC.A21.ES17 as A21E17
import qualified AOC.A21.ES18 as A21E18
import qualified AOC.A21.ES19 as A21E19
import qualified AOC.A21.ES2 as A21E2
import qualified AOC.A21.ES3 as A21E3
import qualified AOC.A21.ES4 as A21E4
import qualified AOC.A21.ES5 as A21E5
import qualified AOC.A21.ES6 as A21E6
import qualified AOC.A21.ES7 as A21E7
import qualified AOC.A21.ES8 as A21E8
import qualified AOC.A21.ES9 as A21E9
import qualified AOC.A22.ES1 as A22E1
import qualified AOC.A22.ES2 as A22E2
import qualified AOC.A20.ES1 as A20E1
import qualified AOC.A20.ES10 as A20E10
import qualified AOC.A20.ES11 as A20E11
import qualified AOC.A20.ES2 as A20E2
import qualified AOC.A20.ES3 as A20E3
import qualified AOC.A20.ES4 as A20E4
import qualified AOC.A20.ES5 as A20E5
import qualified AOC.A20.ES6 as A20E6
import qualified AOC.A20.ES7 as A20E7
import qualified AOC.A20.ES8 as A20E8
import qualified AOC.A20.ES9 as A20E9
import Control.Monad.IO.Class (MonadIO (liftIO))
import Lib.App
import Lib.Exercise
import qualified AOC.A22.ES3 as A22E3
import qualified AOC.A22.ES4 as A22E4
import qualified AOC.A22.ES5 as A22E5
import qualified AOC.A22.ES6 as A22E6
import qualified AOC.A22.ES7 as A22E7
import qualified AOC.A22.ES8 as A22E8

-- Potentially can be Exercise -> Sem r ()
chooseAction :: Exercise -> IO ()
chooseAction (A20E1 fp) = A20E1.main fp
chooseAction (A20E2 fp) = A20E2.main fp
chooseAction (A20E3 fp) = A20E3.main fp
chooseAction (A20E4 fp) = A20E4.main fp
chooseAction (A20E5 fp) = A20E5.main fp
chooseAction (A20E6 fp) = A20E6.main fp
chooseAction (A20E7 fp) = A20E7.main fp
chooseAction (A20E8 fp) = A20E8.main fp
chooseAction (A20E9 fp) = A20E9.main fp
chooseAction (A20E10 fp) = A20E10.main fp
chooseAction (A20E11 fp x) = A20E11.main fp x
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
chooseAction (A21E10 fp) = A21E10.main fp
chooseAction (A21E11 fp) = A21E11.main fp
chooseAction (A21E12 fp) = A21E12.main fp
chooseAction (A21E13 fp) = A21E13.main fp
chooseAction (A21E14 fp) = A21E14.main fp
chooseAction (A21E15 fp) = A21E15.main fp
chooseAction (A21E16 fp) = A21E16.main fp
chooseAction (A21E17 fp) = A21E17.main fp
chooseAction (A21E18 fp) = A21E18.main fp
chooseAction (A21E19 fp) = A21E19.main fp
chooseAction (A22E1 fp) = A22E1.main fp
chooseAction (A22E2 fp) = A22E2.main fp
chooseAction (A22E3 fp) = A22E3.main fp
chooseAction (A22E4 fp) = A22E4.main fp
chooseAction (A22E5 fp) = A22E5.main fp
chooseAction (A22E6 fp) = A22E6.main fp
chooseAction (A22E7 fp) = A22E7.main fp
chooseAction (A22E8 fp) = A22E8.main fp
chooseAction None = liftIO $ putStrLn "Nothing to do"

runExercise :: AppEnv -> IO ()
runExercise env = runApp env $ do
  e <- grab @Exercise
  liftIO . putStrLn $ "Doing exercise " ++ show e
  liftIO $ chooseAction e