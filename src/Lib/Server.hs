module Lib.Server (
    application,
) where

import Lib.App
import Lib.StaticInfo (StaticInfo (..))

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS

{- | This is our main application.
You can swap runApp with a runner that implements (e.g.) logging. When the application
gets non trivial, you can move the monad elsewere and use
```
application env = runner (app env)
```
-}
application :: AppEnv -> IO ()
application env = runApp env $ do
    sInfo <- grab @StaticInfo
    liftIO $ BS.putStrLn $ BS.concat ["Version ", (sVersion sInfo)]
