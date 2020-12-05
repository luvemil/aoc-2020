module Lib (
    main,
) where

import Lib.App
import Lib.Config (
    Config (..),
    loadConfig,
 )
import Lib.Server (application)
import Lib.StaticInfo (StaticInfo (..))

mkAppEnv :: Config -> IO AppEnv
mkAppEnv Config{..} = do
    -- IO configuration

    -- pure configuration
    let sInfo = StaticInfo{sVersion = cVersion}
    pure Env{..}

{- | Basic runner. Here you can fork execution and put a `run "8080" $ application env`
 | to have your server listen on a port.
-}
runServer :: AppEnv -> IO ()
runServer env = application env

main :: IO ()
main = loadConfig >>= mkAppEnv >>= runServer
