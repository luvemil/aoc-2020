module Lib.App.Monad (
    App,
    runApp,
    AppEnv,
) where

import Control.Monad.Reader
import Lib.App.Env (Env)

type AppEnv = Env App

-- | Main application monad.
newtype App a = App
    { unApp :: ReaderT AppEnv IO a
    }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppEnv)

runApp :: AppEnv -> App a -> IO a
runApp env a = runReaderT (unApp a) env
