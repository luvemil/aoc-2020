module Lib.Config (
    Config (..),
    loadConfig,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Toml (
    TomlCodec,
    (.=),
 )

import qualified Toml

-- | Data type for the configurable elements of the application.
data Config = Config
    { cVersion :: !ByteString
    }

-- | TOML codec for the 'Config' data type.
configT :: TomlCodec Config
configT = Config <$> Toml.byteString "version" .= cVersion

-- | Loads the @config.toml@ file.
loadConfig :: MonadIO m => m Config
loadConfig = Toml.decodeFile configT "config.toml"
