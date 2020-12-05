module Lib (
    main,
) where

import AOC.Main (runExercise)
import Lib.App
import Lib.Config (
    Config (..),
    loadConfig,
 )
import Lib.StaticInfo (StaticInfo (..))

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration

    -- pure configuration
    let sInfo = StaticInfo{sVersion = cVersion}
    pure Env{..}

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runExercise
