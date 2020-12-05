{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Options.Generic

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration
    ex <- getRecord "Exercise"

    -- pure configuration
    let sInfo = StaticInfo{sVersion = cVersion}
        exercise = ex
    pure Env{..}

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runExercise
